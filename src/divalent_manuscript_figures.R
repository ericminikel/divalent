# STARTUP #### 

overall_start_time = Sys.time()
tell_user = function(...) { cat(file=stderr(), paste0(...)); flush.console() }

# DEPENDENCIES ####

tell_user('Loading required packages...')

options(stringsAsFactors=F)
if (interactive()) {
  setwd('~/d/sci/src/divalent')
}
suppressPackageStartupMessages({
  library(tidyverse)
  library(survival)
  library(janitor)
  library(openxlsx)
  library(DescTools)
  library(magick)
  library(drc)
  select = dplyr::select
  summarize = dplyr::summarize
})


# OUTPUT STREAMS #### 

tell_user('done.\nCreating output streams...')

text_stats_path = 'display_items/stats_for_text.txt'
write(paste('Last updated: ',Sys.Date(),'\n',sep=''),text_stats_path,append=F) # start anew - but all subsequent writings will be append=T
write_stats = function(...) {
  write(paste(list(...),collapse='',sep=''),text_stats_path,append=T)
  write('\n',text_stats_path,append=T)
}

supplement_path = 'display_items/supplement.xlsx'
supplement = createWorkbook()
# options("openxlsx.numFmt" = "0.00") # this looks better for residuals but terrible for p values and weeks post-dose
supplement_directory = tibble(name=character(0), title=character(0))
write_supp_table = function(tbl, title='') {
  # write Excel sheet for supplement
  table_number = length(names(supplement)) + 1
  table_name = paste0('s',formatC(table_number,'d',digits=0,width=2,flag='0'))
  addWorksheet(supplement,table_name)
  bold_style = createStyle(textDecoration = "Bold")
  writeData(supplement,table_name,tbl,headerStyle=bold_style,withFilter=T)
  freezePane(supplement,table_name,firstRow=T)
  saveWorkbook(supplement,supplement_path,overwrite = TRUE)
  
  # also write tab-sep version for GitHub repo
  write_tsv(tbl,paste0('display_items/table-',table_name,'.tsv'), na='')
  
  # and save the title in the directory tibble for later
  assign('supplement_directory',
         supplement_directory %>% add_row(name=table_name, title=title),
         envir = .GlobalEnv)
}




# FUNCTIONS ####

tell_user('done.\nDefining functions...')

clipcopy = function(tbl) {
  clip = pipe("pbcopy", "w")  
  write.table(tbl, file=clip, sep = '\t', quote=F, row.names = F, na='')
  close(clip)
}

rank_uniq = function(x) match(x, sort(unique(x)))

percent = function(x, digits=0, signed=F) gsub(' ','',paste0(ifelse(x <= 0, '', ifelse(signed, '+', '')),formatC(100*x,format='f',digits=digits),'%'))

upper = function(x, ci=0.95) { 
  alpha = 1 - ci
  sds = qnorm(1-alpha/2)
  mean(x) + sds*sd(x)/sqrt(sum(!is.na(x)))
}

lower = function(x, ci=0.95) { 
  alpha = 1 - ci
  sds = qnorm(1-alpha/2)
  mean(x) - sds*sd(x)/sqrt(sum(!is.na(x)))
}

alpha = function(rgb_hexcolor, proportion) {
  hex_proportion = sprintf("%02x",round(proportion*255))
  rgba = paste(rgb_hexcolor,hex_proportion,sep='')
  return (rgba)
}
ci_alpha = 0.35 # degree of transparency for shading confidence intervals in plot


rbind_files = function(path, grepstring) {
  all_files = list.files(path, full.names=T)
  these_files = all_files[grepl(grepstring,all_files)]
  if (exists('rbound_table')) rm('rbound_table')
  for (this_file in these_files) {
    this_tbl = read_delim(this_file, col_types=cols()) %>% clean_names()
    this_tbl$file = gsub('.*\\/','',gsub('\\.[tc]sv','',this_file))
    if (exists('rbound_table')) {
      rbound_table = rbind(rbound_table, this_tbl)
    } else {
      rbound_table = this_tbl
    }
  }
  return (rbound_table)
}


weight_deltas = function(study_ids) {
  
  study %>%
    filter(study_id %in% study_ids) %>%
    inner_join(wts, by=c('study_id','animal')) %>%
    mutate(wt_date = as.integer(date - icv_date)) %>%
    select(study_id, animal, sex, tx, dose_dio, wt_date, weight) %>%
    group_by(animal) %>%
    mutate(baseline=weight[wt_date==min(wt_date)]) %>%
    ungroup() %>%
    mutate(change = weight/baseline-1) -> deltas
  
  return(deltas)
}

process_elisas = function(study_ids, control_group='saline') {
  study %>%
    filter(study_id %in% study_ids) %>%
    inner_join(elisa, by=c('study_id','animal'='sample')) %>%
    group_by(plate, genotype) %>%
    mutate(saline_mean = mean(ngml_av[tx %in% control_group], na.rm=T),
           n_saline = sum(tx %in% control_group & !is.na(ngml_av))) %>%
    ungroup() %>%
    mutate(rel = ngml_av / saline_mean) %>%
    select(study_id, plate, animal, genotype, sex, tx, dose_dio, dosing_regimen, days_harvest, ngml_av, saline_mean, n_saline, rel) -> proc
}


dviz = function(tbl,
                xlims,
                ylims,
                xvar,
                yvar,
                colorvar=NULL,
                pchvar=NULL,
                pchbg='#FFFFFF',
                pchout=NULL,
                pchcex=1,
                xcols=character(0), # columns that group with x TBD
                xats=xbigs,
                xbigs,
                xlwds=1,
                xbiglabs=xbigs,
                xaxcex=1,
                xlabcex=1,
                xlabline=1.5,
                yats=ybigs,
                ybigs,
                ylwds=1,
                ybiglabs=ybigs,
                yaxcex=1,
                ylabcex=1,
                ylabline=2.25,
                log,
                mar=c(3,3,1,1),
                jitamt=0.1,
                randseed=1,
                boxwidth=NA,
                barwidth=NA,
                bartype='segment',
                polygon=NA,
                arrowlength=0.05,
                test=NA,
                control_group=NA,
                xlab='',
                ylab='',
                legtext=NULL,
                legcol='#000000',
                legtextcol=legcol,
                leglty=1,
                legpch=20,
                leglwd=1,
                legcex=1,
                legloc=NULL,
                crosshairs=F
) {
  
  if (is.null(pchvar)) {
    pchvar='pch'
    tbl$pch = 19
  }
  
  if (is.null(colorvar)) {
    colorvar='color'
    tbl$color = '#000000'
  }
  
  tbl %>%
    mutate(x=!!as.name(xvar), y=!!as.name(yvar), color=!!as.name(colorvar), pch=!!as.name(pchvar)) %>%
    select(x, y, color, pch, all_of(xcols)) -> tbl
  
  if (!crosshairs) {
    xcols = c('x', xcols)
  }
  
  tbl %>%
    group_by(color, across(all_of(xcols))) %>%
    summarize(.groups='keep',
              n = n(),
              mean = mean(y),
              l95 = mean(y) - 1.96 * sd(y) / sqrt(n()),
              u95 = mean(y) + 1.96 * sd(y) / sqrt(n()),
              median = median(y),
              q25 = quantile(y, .25),
              q75 = quantile(y, .75),
              cv = sd(y) / mean(y),
              x_mean = mean(x),
              x_l95  = mean(x) - 1.96 * sd(x) / sqrt(n()),
              x_u95  = mean(x) + 1.96 * sd(x) / sqrt(n())) %>%
    ungroup() %>%
    mutate(l95 = ifelse(l95 < 0 & log %in% c('xy','y'), min(ylims), l95)) %>%
    #mutate(x = case_when(crosshairs ~ x_mean, 
    #                     TRUE ~ x)) %>%
    arrange(x) -> tbl_smry
  
  if (crosshairs) {
    tbl_smry$x = tbl_smry$x_mean
  }
  
  par(mar=mar)
  plot(NA, NA, xlim=xlims, ylim=ylims, axes=F, ann=F, xaxs='i', yaxs='i', log=log)
  axis(side=1, at=xlims, labels=NA, lwd.ticks=0)
  if (!is.null(xats)) {
    axis(side=1, at=xats, tck=-0.025, lwd.ticks=xlwds, labels=NA)
  }
  if (!is.null(xbigs)) {
    axis(side=1, at=xbigs, tck=-0.05, lwd.ticks=xlwds, labels=NA)
    axis(side=1, at=xbigs, tck=-0.05, lwd=0, line=-0.5, labels=xbiglabs, cex.axis=xaxcex)
  }
  mtext(side=1, line=xlabline, text=xlab, cex=xlabcex)
  if (!is.null(yats)) {
    axis(side=2, at=yats, tck=-0.025, labels=NA)
  }
  if (!is.null(ybigs)) {
    axis(side=2, at=ybigs, tck=-0.05, labels=NA)
    axis(side=2, at=ybigs, tck=-0.05, las=2, lwd=0, line=-0.3, labels=ybiglabs, cex.axis=yaxcex)
  }
  mtext(side=2, line=ylabline, text=ylab, cex=ylabcex)
  if (crosshairs) {
    jitamt = 0
  }

  if (crosshairs) {
    segments(x0=tbl_smry$x_l95, x1=tbl_smry$x_u95, y0=tbl_smry$mean, col=tbl_smry$color, lwd=1.5)
    segments(x0=tbl_smry$x_mean, y0=tbl_smry$l95, y1=tbl_smry$u95, col=tbl_smry$color, lwd=1.5)
  }
  if (!is.na(barwidth)) {
    if (bartype=='segment') {
      segments(x0=tbl_smry$x-barwidth, x1=tbl_smry$x+barwidth, y0=tbl_smry$mean, col=tbl_smry$color, lwd=1.5)
    } else if (bartype=='bar') {
      rect(xleft=tbl_smry$x-barwidth, xright=tbl_smry$x+barwidth, ybottom=rep(0,nrow(tbl_smry)), ytop=tbl_smry$mean, col=tbl_smry$color, border=NA)
    }
    arrows(x0=tbl_smry$x, y0=tbl_smry$l95, y1=tbl_smry$u95, code=3, angle=90, length=arrowlength, col=tbl_smry$color, lwd=1.5)
  }
  if (!is.na(boxwidth)) {
    rect(xleft=tbl_smry$x-boxwidth, xright=tbl_smry$x+boxwidth, ybottom=tbl_smry$q25, ytop=tbl_smry$q75, border=tbl_smry$color, lwd=1.5, col=NA)
    segments(x0=tbl_smry$x-boxwidth, x1=tbl_smry$x+boxwidth, y0=tbl_smry$median, col=tbl_smry$color, lwd=1.5)
  }
  
  set.seed(randseed)
  if (!is.null(pchout)) {
    points(x=jitter(tbl$x,amount=jitamt), y=tbl$y, col=pchout, pch=tbl$pch, bg=pchbg)
  } else {
    points(x=jitter(tbl$x,amount=jitamt), y=tbl$y, col=alpha(tbl$color,ci_alpha), pch=tbl$pch, bg=pchbg) 
  }
  
  
  
  if (!is.na(polygon)) {
    for (clr in unique(tbl_smry$color)) {
      if (polygon=='iqr') {
        subs = subset(tbl_smry, color==clr & !is.na(q25) & !is.na(q75))
        points( x=  subs$x, y=  subs$median, type='l', lwd=2, col=subs$color)
        polygon(x=c(subs$x, rev(subs$x)), y=c(subs$q25, rev(subs$q75)), col=alpha(subs$color, ci_alpha), border=NA)
      } else if (polygon=='ci') {
        subs = subset(tbl_smry, color==clr & !is.na(l95) & !is.na(u95))
        points( x=  subs$x, y=  subs$mean, type='l', lwd=2, col=subs$color)
        polygon(x=c(subs$x, rev(subs$x)), y=c(subs$l95, rev(subs$u95)), col=alpha(subs$color, ci_alpha), border=NA)
      }
    }
  }
  
  if (!is.na(test)) {
    testfun = get(test) # e.g. ks.test
    control_color = control_group
    tbl_smry$p = as.numeric(NA)
    for (i in 1:nrow(tbl_smry)) {
      this_x = tbl_smry$x[i]
      this_rows = tbl$x == this_x & tbl$color == tbl_smry$color[i]
      ctrl_rows = tbl$x == this_x & tbl$color == control_group
      test_obj = suppressWarnings(testfun(tbl$y[this_rows],  tbl$y[ctrl_rows]))
      tbl_smry$p[i] = test_obj$p.value
    }
    tbl_smry$p_symb = ''
    tbl_smry$p_symb[!is.na(tbl_smry$p) & tbl_smry$p < 0.05] = '*'
    tbl_smry$p_symb[!is.na(tbl_smry$p) & tbl_smry$p < 0.01] = '**'
    tbl_smry$p_symb[!is.na(tbl_smry$p) & tbl_smry$p < 0.001] = '***'
    text(x=tbl_smry$x[tbl_smry$color != control_color], y=max(ylims)*.95, labels=tbl_smry$p_symb[tbl_smry$color != control_color])
  }
  
  if (!is.null(legtext)) {
    if (is.null(legloc)) {
      par(xpd=T)
      legend(x=max(xlims),y=max(ylims),legtext,col=legcol,text.col=legtextcol,pch=legpch,lwd=leglwd, cex=legcex,lty=leglty, bty='n')
      par(xpd=F)
    } else {
      if (length(legloc)==1) {
        legend(legloc,legtext,col=legcol,text.col=legtextcol,pch=legpch,lwd=leglwd, cex=legcex,lty=leglty, bty='n')
      } else if (length(legloc)==2) {
        legend(x=legloc[1],y=legloc[2],legtext,col=legcol,text.col=legtextcol,pch=legpch,lwd=leglwd, cex=legcex,lty=leglty, bty='n')
      }
    }
  }
  
  return(tbl_smry)
  
}



# rcomp = function(x) {
#   chartr('ACGT','TGCA',paste0(rev(strsplit(gsub('U','T',toupper(x)),split='')[[1]]), collapse=''))
# }

rcompu_unary = function(x) {
  chartr('ACGU','UGCA',paste0(rev(strsplit(x,split='')[[1]]), collapse=''))
}


rcompu = function(x) {
  result = character(length(x))
  for (i in 1:length(x)) {
    result[i] = rcompu_unary(x[i])
  }
  return (result)
}



dna_to_rna = function(x) {
  chartr('T','U',x)
}

rna_to_dna = function(x) {
  chartr('U','T',x)
}

p_to_symbol = function(p) {
  case_when(p < 0.001 ~ '***',
            p < 0.01 ~ '**',
            p < 0.05 ~ '*',
            TRUE ~ '')
}

# DATA #### 

tell_user('done.\nReading in data...')

## Target engagement studies #### 
study = read_tsv('data/analytic/studysheets.tsv', col_types=cols())
elisa = read_tsv('data/analytic/elisa.tsv', col_types=cols())
qpcr = read_tsv('data/analytic/qpcr.tsv', col_types=cols(), guess_max=10000)
wts = read_tsv('data/analytic/weights.tsv', col_types=cols(), guess_max=10000)
meta = read_tsv('data/analytic/meta.tsv', col_types=cols())
cellulo = read_tsv('data/analytic/in_cellulo.tsv', col_types=cols())

# renaming some things
# qpcr %>%
#   distinct(study_id, sample_group) -> qpcr_txes
# write_tsv(qpcr_txes, 'data/analytic/qpcr_txes_start.tsv', na='')
# annotated in google sheets, read back in
qpcr_txes = read_tsv('data/analytic/qpcr_txes.tsv', col_types=cols())

## Challenge study #### 
yb_blind = read_tsv('data/challenge/YB_blind.tsv', col_types=cols())
yb_master = read_tsv('data/challenge/YB_master.tsv', col_types=cols())
yb_meta = read_tsv('data/challenge/YB_meta.tsv', col_types=cols())
yb_nests = read_tsv('data/challenge/YB_nests.tsv', col_types=cols())
yb_surg = read_tsv('data/challenge/YB_surgery_notes.tsv', col_types=cols())
yb_wts = read_tsv('data/challenge/YB_weights.tsv', col_types=cols())

## Constants #### 

mw = 24952
empirical_mec = 549107
theoretical_mec = 766260
dose_correction = theoretical_mec/empirical_mec
nmol_to_ug = function(x, digits=0) { round(x * 1e-9 * mw * dose_correction * 1e6,digits=digits) }

region_meta = tibble(region=c("HP", "PFC", "VC", "Str", "Thal", "CB"),
                     x = 1:6)

# table(study$tx)

# mouse_init = read_tsv('data/miscellaneous/mouse_initial_screen.tsv', col_types=cols())

huseq_raw = read_tsv('data/sequence/Homo_sapiens_NM_000311_4_sequence.fa', skip=1, col_names = c('seq'), col_types=cols())
huseq = paste0(huseq_raw$seq,collapse='')


# DISPLAY ITEMS #### 

## Supplementary tables ####

sirna_sequences = read_tsv('data/supptables/antisense_sequences.tsv', col_types=cols())
write_supp_table(sirna_sequences, 'Antisense strand sequences of all siRNAs screened.')

all_studies = c("CMR-1313", "CMR-1403", "CMR-1418", "CMR-1465", "CMR-1656", 
  "CMR-1697", "CMR-1871", "CMR-2198", "CMR-2371", "CMR-2521", "CMR-2752", 
  "CMR-2812", "CMR-2833", "CMR-2941", "CMR-3164")
proc_all = process_elisas(all_studies, control_group=c('saline','none','no injection','-')) %>%
  inner_join(meta, by='tx') %>%
  rename(prp_ngml = ngml_av, untreated_mean = saline_mean) %>%
  select(study_id, plate, animal, genotype, sex, display_tx, dose_dio, dosing_regimen, days_harvest, prp_ngml, untreated_mean, rel) %>%
  mutate(genotype = case_when(genotype=='Tg26372 HOM/ZH3 HOM' ~ 'Tg26372',
                              T ~ genotype))

proc_all %>%
  mutate(weeks_harvest = days_harvest %/% 7) %>%
  group_by(study_id, genotype, display_tx, dose_dio, dosing_regimen, weeks_harvest) %>%
  summarize(.groups='keep',
            n = n(),
            mean = mean(rel),
            l95 = lower(rel),
            u95 = upper(rel)) %>%
  ungroup() -> proc_all_smry

write_supp_table(proc_all, 'All in vivo PrP ELISA results - individual animal data.')
write_supp_table(proc_all_smry, 'All in vivo PrP ELISA results - summarized.')

# note: things that appear only in these supp tables
# - 1 nmol dio tests of 1035 and 2440



## Fig S2 - Zack's data #### 
tell_user('done.\nCreating Figure S2...')

if ('figure-s2'=='figure-s2') {
  

resx=300
png('display_items/figure-s2.png',width=6.5*resx,height=6*resx,res=resx)



layout_matrix = matrix(c(1,1,2,3,
                         1,1,4,5,
                         6,6,7,8,
                         6,6,9,10), nrow=4, byrow=T)
layout(layout_matrix)

panel = 1

### mouse screen ####

species_meta = tibble(species=c('Ms','Hs','Hs_Ms'),
                      disp = c('mouse-only','human-only','cross-reactive'))

z_mo_scr = read_tsv('data/miscellaneous/mouse_screen.tsv', col_types=cols())

z_mo_scr %>%
  filter(!is.na(ratio)) %>%
  filter(species=='Hs') %>% # predicted non-reactive
  group_by(descno) %>%
  summarize(.groups='keep', n_replicates = n(), mean=mean(ratio)) %>%
  ungroup() %>%
  filter(n_replicates > 1) %>%
  summarize(ntc_mean = mean(mean)) %>%
  pull() -> ntc_mean

ylims = c(0,1.5)

z_mo_scr %>%
  filter(!is.na(ratio)) %>%
  inner_join(species_meta, by='species') %>%
  mutate(resid = pmin(ratio / ntc_mean, max(ylims))) %>%
  arrange(disp, descno) %>%
  mutate(x = dense_rank(paste0(disp, formatC(descno,width=4,flag='0')))) -> scr


scr %>%
  group_by(x, disp, descno) %>%
  summarize(.groups='keep', 
            mean = mean(resid),
            l95 = lower(resid),
            u95 = upper(resid)) %>%
  ungroup() -> scr_smry

scr_smry %>%
  group_by(disp) %>%
  summarize(.groups='keep',
            minx = min(x),
            midx = mean(x),
            maxx = max(x)) %>%
  ungroup() -> tranches

xlims = range(scr_smry$x) + c(-0.5, 0.5)

ybigs = 0:3/2
ybiglabs = c('0%','50%','100%','≥150%')

par(mar=c(4,4,2,1))
plot(NA, NA, xlim=xlims, ylim=ylims, axes=F, ann=F, xaxs='i', yaxs='i')
axis(side=1, at=xlims, lwd.ticks=0, labels=NA)
axis(side=2, at=ybigs, labels=NA)
axis(side=2, at=ybigs, labels=ybiglabs, cex.axis=0.8, las=2, line=-0.5, lwd=0)
mtext(side=2, line=2.5, text='residual Prnp')
axis(side=3, at=xlims, lwd.ticks=0, labels=NA)
abline(h=1, lty=3)
barwidth = 0.8
rect(xleft=scr_smry$x-barwidth/2, xright=scr_smry$x+barwidth/2,
     ybottom=rep(0,nrow(scr_smry)), ytop=scr_smry$mean, col=alpha('#000000',ci_alpha),
     border=NA)
arrows(x0=scr_smry$x, y0=scr_smry$l95, y1=scr_smry$u95, angle=90, length=0.05, code=3)
points(scr$x, scr$resid, pch=21, bg='#FFFFFF')
mtext(side=1, at=scr_smry$x, text=scr_smry$descno, las=2, cex=0.5)
tranche_line = 1.75
overhang_left = 0.4
overhang_right = 0.4
for (i in 1:nrow(tranches)) {
  axis(side=1, line=tranche_line, at=c(tranches$minx[i], tranches$maxx[i]) + c(-1,1)*c(overhang_left,overhang_right), tck=0.03, labels=NA)
  mtext(side=1, line=tranche_line+0.2, at=tranches$midx[i], text=tranches$disp[i], cex=0.8)
}
mtext(side=3, adj=0, text='mouse N2a cells', line=0.5)
mtext(side=3, adj=-0.2, text=LETTERS[panel], line=0.5); panel = panel + 1

scr_smry %>%
  select(-x) %>%
  rename(species=disp) %>%
  relocate(descno) -> scr_smry_out
write_supp_table(scr_smry_out, 'Mouse siRNA sequence screen in N2a cells with bDNA assay readout.')

### mouse IC50 ####

z_ic50_mo = read_tsv('data/miscellaneous/ic50_mo.tsv', col_types=cols())

z_ic50 = z_ic50_mo
cpds = unique(z_ic50_mo$compound)

par(mar=c(2,3,1,1))
i = 1

for (cpd in cpds) {
  
  if (i == 1) {
    par(mar=c(0.5,3,3,1))
  } else if (i == 2) {
    par(mar=c(0.5,1,3,3))
  } else if (i == 3) {
    par(mar=c(3,3,0.5,1))
  } else if (i == 4) {
    par(mar=c(3,1,0.5,3))
  }
  
  ic50_points = z_ic50 %>%
    subset(compound==cpd) %>%
    rename(resid = normed, concnm = dose)
  
  ylims = c(0, 1.5)
  xlims = range(ic50_points$concnm[ic50_points$concnm > 0]) * c(1/3.16, 3.16)
  xbigs = 10^(-3:3)
  xbiglabs = -3:3
  xats = rep(1:9, 7) * rep(10^(-3:3), each=9)
  ybigs = 0:3/2
  ybiglabs = c('0%','50%','100%','≥150%')
  
  plot(NA, NA, xlim=xlims, ylim=ylims, xaxs='i', yaxs='i', ann=F, axes=F, log='x')
  axis(side=1, at=xlims, lwd.ticks=0, labels=NA)
  axis(side=1, at=xbigs, labels=NA, tck=-0.05)
  axis(side=1, at=xats, labels=NA, tck=-0.02)
  axis(side=2, at=ylims, labels=NA, lwd.ticks=0)
  if (i %% 2 == 1) { # first in each row
    axis(side=2, at=ybigs, labels=NA, tck=0.025)
    axis(side=2, at=ybigs, labels=ybiglabs, lwd=0, line=-.75, las=2, cex.axis=.75)
    
  }
  if (i > length(cpds)/2) { # bottom row
    axis(side=1, at=xbigs, labels=xbiglabs, lwd.ticks=0, lwd=0, line=-1.0, cex.axis=.7)
    mtext(side=1, line=1, text='log10(nM)', cex=0.6)
  }
  if (i==3) { # bottom left
    mtext(side=2, line=2.5, at=max(ylims), text='residual Prnp', cex=.8)
  }
  abline(h=c(0.5,1), lty=3, lwd=0.5)
  points(x=ic50_points$concnm, y=pmin(ic50_points$resid,max(ylims)), pch=20, cex=0.5)
  if (cpd != 1035) { # had to hard-code this because drm() non-convergence throws a tryCatch-resistant error that stops execution
    m = drm(resid ~ concnm, fct=LL.4(fixed=c(b=NA,c=0,d=1,e=NA)), data=ic50_points %>% mutate(resid = pmin(resid, max(ylims))))
    ic50 = pmin(m$coefficients['e:(Intercept)'], max(ic50_points$concnm))
    plot(m, type='none', add=T)
  } else {
    ic50 = max(ic50_points$concnm)
  }
  greater_than = ifelse(ic50==max(ic50_points$concnm),'≥','')
  mtext(side=1, line=-2, adj=0, text=paste0('  ',cpd), cex=0.7)
  mtext(side=1, line=-1, adj=0, text=paste0('  ',greater_than,formatC(ic50,format='f',digits=0),' nM'), cex=0.7)
  mtext(side=3, adj=0.05, text=LETTERS[panel], line=-1); panel = panel + 1
  i = i + 1
}

ic50_points %>%
  rename(descno = compound) %>%
  select(descno, concnm, resid) -> ic50_points_out
write_supp_table(ic50_points_out, 'Mouse siRNA sequence IC50 determination in N2a cells with bDNA assay readout.')

### human screen #### 


z_hu_scr = read_tsv('data/miscellaneous/human_screen.tsv', col_types=cols())

z_hu_scr %>%
  filter(!is.na(ratio)) %>%
  filter(species=='Ms') %>% # predicted non-reactive
  group_by(descno) %>%
  summarize(.groups='keep', n_replicates = n(), mean=mean(ratio)) %>%
  ungroup() %>%
  filter(n_replicates > 1) %>%
  summarize(ntc_mean = mean(mean)) %>%
  pull() -> ntc_mean

ylims = c(0,1.5)

z_hu_scr %>%
  filter(!is.na(ratio)) %>%
  inner_join(species_meta, by='species') %>%
  mutate(resid = pmin(ratio / ntc_mean, max(ylims))) %>%
  arrange(disp, descno) %>%
  mutate(x = dense_rank(paste0(disp, formatC(descno,width=4,flag='0')))) -> scr


scr %>%
  group_by(x, disp, descno) %>%
  summarize(.groups='keep', 
            mean = mean(resid),
            l95 = lower(resid),
            u95 = upper(resid)) %>%
  ungroup() -> scr_smry

scr_smry %>%
  group_by(disp) %>%
  summarize(.groups='keep',
            minx = min(x),
            midx = mean(x),
            maxx = max(x)) %>%
  ungroup() -> tranches

xlims = range(scr_smry$x) + c(-0.5, 0.5)

ybigs = 0:3/2
ybiglabs = c('0%','50%','100%','≥150%')

par(mar=c(4,4,2,1))
plot(NA, NA, xlim=xlims, ylim=ylims, axes=F, ann=F, xaxs='i', yaxs='i')
axis(side=1, at=xlims, lwd.ticks=0, labels=NA)
axis(side=2, at=ybigs, labels=NA)
axis(side=2, at=ybigs, labels=ybiglabs, cex.axis=0.8, las=2, line=-0.5, lwd=0)
mtext(side=2, line=2.5, text='residual Prnp')
axis(side=3, at=xlims, lwd.ticks=0, labels=NA)
abline(h=1, lty=3)
barwidth = 0.8
rect(xleft=scr_smry$x-barwidth/2, xright=scr_smry$x+barwidth/2,
     ybottom=rep(0,nrow(scr_smry)), ytop=scr_smry$mean, col=alpha('#000000',ci_alpha),
     border=NA)
arrows(x0=scr_smry$x, y0=scr_smry$l95, y1=scr_smry$u95, angle=90, length=0.05, code=3)
points(scr$x, scr$resid, pch=21, bg='#FFFFFF')
mtext(side=1, at=scr_smry$x, text=scr_smry$descno, las=2, cex=0.5)
tranche_line = 1.75
overhang_left = 0.4
overhang_right = 0.4
for (i in 1:nrow(tranches)) {
  axis(side=1, line=tranche_line, at=c(tranches$minx[i], tranches$maxx[i]) + c(-1,1)*c(overhang_left,overhang_right), tck=0.03, labels=NA)
  mtext(side=1, line=tranche_line+0.2, at=tranches$midx[i], text=tranches$disp[i], cex=0.8)
}
mtext(side=3, adj=0, text='human A549 cells', line=0.5)
mtext(side=3, adj=-0.2, text=LETTERS[panel], line=0.5); panel = panel + 1

scr_smry %>%
  select(-x) %>%
  rename(species=disp) %>%
  relocate(descno) -> scr_smry_out
write_supp_table(scr_smry_out, 'Human siRNA sequence screen in A549 cells with bDNA assay readout.')

### human IC50 #### 

z_ic50_hu = read_tsv('data/miscellaneous/ic50_hu.tsv', col_types=cols())

z_ic50 = z_ic50_hu
cpds = unique(z_ic50_hu$compound)

par(mar=c(2,3,1,1))
i = 1

for (cpd in cpds) {
  
  if (i == 1) {
    par(mar=c(0.5,3,3,1))
  } else if (i == 2) {
    par(mar=c(0.5,1,3,3))
  } else if (i == 3) {
    par(mar=c(3,3,0.5,1))
  } else if (i == 4) {
    par(mar=c(3,1,0.5,3))
  }
  
  ic50_points = z_ic50 %>%
    subset(compound==cpd) %>%
    rename(resid = normed, concnm = dose)
  
  ylims = c(0, 1.5)
  xlims = range(ic50_points$concnm[ic50_points$concnm > 0]) * c(1/3.16, 3.16)
  xbigs = 10^(-3:3)
  xbiglabs = -3:3
  xats = rep(1:9, 7) * rep(10^(-3:3), each=9)
  ybigs = 0:3/2
  ybiglabs = c('0%','50%','100%','≥150%')
  
  plot(NA, NA, xlim=xlims, ylim=ylims, xaxs='i', yaxs='i', ann=F, axes=F, log='x')
  axis(side=1, at=xlims, lwd.ticks=0, labels=NA)
  axis(side=1, at=xbigs, labels=NA, tck=-0.05)
  axis(side=1, at=xats, labels=NA, tck=-0.02)
  axis(side=2, at=ylims, labels=NA, lwd.ticks=0)
  if (i %% 2 == 1) { # first in each row
    axis(side=2, at=ybigs, labels=NA, tck=0.025)
    axis(side=2, at=ybigs, labels=ybiglabs, lwd=0, line=-.75, las=2, cex.axis=.75)
    
  }
  if (i > length(cpds)/2) { # bottom row
    axis(side=1, at=xbigs, labels=xbiglabs, lwd.ticks=0, lwd=0, line=-1.0, cex.axis=.7)
    mtext(side=1, line=1, text='log10(nM)', cex=0.6)
  }
  if (i==3) { # bottom left
    mtext(side=2, line=2.5, at=max(ylims), text='residual Prnp', cex=.8)
  }
  abline(h=c(0.5,1), lty=3, lwd=0.5)
  points(x=ic50_points$concnm, y=pmin(ic50_points$resid,max(ylims)), pch=20, cex=0.5)
  if (cpd != 1035) { # had to hard-code this because drm() non-convergence throws a tryCatch-resistant error that stops execution
    m = drm(resid ~ concnm, fct=LL.4(fixed=c(b=NA,c=0,d=1,e=NA)), data=ic50_points %>% mutate(resid = pmin(resid, max(ylims))))
    ic50 = pmin(m$coefficients['e:(Intercept)'], max(ic50_points$concnm))
    plot(m, type='none', add=T)
  } else {
    ic50 = max(ic50_points$concnm)
  }
  greater_than = ifelse(ic50==max(ic50_points$concnm),'≥','')
  mtext(side=1, line=-2, adj=0, text=paste0('  ',cpd), cex=0.7)
  mtext(side=1, line=-1, adj=0, text=paste0('  ',greater_than,formatC(ic50,format='f',digits=0),' nM'), cex=0.7)
  mtext(side=3, adj=0.05, text=LETTERS[panel], line=-1); panel = panel + 1
  i = i + 1
}

ic50_points %>%
  rename(descno = compound) %>%
  select(descno, concnm, resid) -> ic50_points_out
write_supp_table(ic50_points_out, 'Human siRNA sequence IC50 determination in A549 cells with bDNA assay readout.')

silence_is_golden = dev.off()
}


# check that species cross-reactivity never contradict each other
# 
# z_mo_scr %>% distinct(descno, species) -> mo_desig
# z_hu_scr %>% distinct(descno, species) -> hu_desig
# 
# mo_desig %>%
#   full_join(hu_desig, by='descno', suffix=c('_mo','_hu')) %>%
#   filter(species_mo == species_hu)

## Figure S3. Broad mouse reagent screening #### 
tell_user('done.\nCreating Figure S3...')
if ('figure-s3'=='figure-s3') {
  

resx=300
png('display_items/figure-s3.png',width=6.5*resx,height=3.5*resx,res=resx)

layout_matrix = matrix(c(1, 1, 2, 3,
                         1, 1, 4, 5), nrow=2, byrow=T)
layout(layout_matrix, widths=c(1, rep(.5, .5)))

par(mar=c(5,5,3,1))

panel = 1
# 
# ylims = c(0,1.5)
# mouse_initial_screen_raw = read_tsv('data/miscellaneous/mouse_initial_screen.tsv', col_types=cols())
# 
# mouse_initial_screen_raw %>%
#   group_by(sequence_number) %>%
#   summarize(.groups = 'keep',
#             mean = mean(residual), 
#             l95  = lower(residual),
#             u95  = upper(residual),
#             cv = sd(residual)/mean(residual)) %>%
#   ungroup() -> mouse_initial_screen_stats
# 
# mouse_initial_screen_raw %>%
#   anti_join(mouse_initial_screen_stats %>% filter(cv > 0.75), by='sequence_number') %>%
#   mutate(x = rank_uniq(sequence_number)) %>%
#   mutate(color = '#000000') -> init
# 
# init %>%
#   group_by(x, sequence_number, color) %>%
#   summarize(.groups='keep',
#             mean = mean(residual),
#             l95 = lower(residual), 
#             u95 = upper(residual)) %>%
#   ungroup() -> init_smry
# 
# xlims = range(init$x + c(-0.5, 0.5))
# ylims = c(0, 1.5)
# yats = 0:6/4
# ybigs = 0:3/2
# plot(NA, NA, xlim=xlims, ylim=ylims, axes=F, ann=F, xaxs='i', yaxs='i')
# axis(side=1, at=xlims, lwd.ticks=0, labels=NA)
# mtext(side=1, at=init_smry$x, text=init_smry$sequence_number, las=2, line=0.25)
# axis(side=2, at=yats, labels=NA, tck=-0.02)
# axis(side=2, at=ybigs, labels=NA, tck=-0.05)
# axis(side=2, at=ybigs, labels=percent(ybigs), lwd=0, line=-0.5, las=2)
# abline(h=1, lty=3)
# points(x=init$x, y=pmin(init$residual,max(ylims)), col=alpha(init$color, ci_alpha), pch=19)
# barwidth = 0.5
# rect(xleft=init_smry$x - barwidth/2, xright=init_smry$x + barwidth/2, ybottom=rep(0, nrow(init_smry)), ytop=init_smry$mean, col=alpha(init_smry$color, ci_alpha), border=NA)
# arrows(x0=init_smry$x, y0=init_smry$l95, y1=init_smry$u95, code=3, angle=90, length=0.05, lwd=1.5)

cellulo %>%
  filter(purpose=='screen' & cells=='N2a' & concnm == 2000 & sample_group != 'untreated' & !is.na(descno)) %>%
  mutate(color = '#000000') %>%
  mutate(descno = as.integer(descno)) %>%
  mutate(x = rank_uniq(descno)) %>% 
  group_by(x, descno, concnm, biorep, color) %>% # group first across techreps within biorep
  summarize(.groups='keep',
            resid = mean(resid)) %>%
  ungroup() -> mouse_screen

mouse_screen %>%
  group_by(x, descno, concnm, color) %>% # now group across bioreps
  summarize(.groups = 'keep',
            n = n(),
            mean = mean(resid), 
            l95  = lower(resid),
            u95  = upper(resid)) %>%
  ungroup() -> mouse_screen_smry

mouse_screen_smry %>%
  select(descno, concnm, mean, l95, u95) -> mouse_screen_smry_out
write_supp_table(mouse_screen_smry_out, 'Mouse siRNA sequence screen in N2a cells with qPCR readout.')


xlims = range(mouse_screen$x + c(-0.5, 0.5))
ylims = c(0, 1.5)
yats = 0:6/4
ybigs = 0:3/2
plot(NA, NA, xlim=xlims, ylim=ylims, axes=F, ann=F, xaxs='i', yaxs='i')
axis(side=1, at=xlims, lwd.ticks=0, labels=NA)
mtext(side=1, at=mouse_screen_smry$x, text=mouse_screen_smry$descno, las=2, line=0.25)
axis(side=2, at=yats, labels=NA, tck=-0.02)
axis(side=2, at=ybigs, labels=NA, tck=-0.05)
axis(side=2, at=ybigs, labels=percent(ybigs), lwd=0, line=0, las=2)
mtext(side=2, line=3.0, text='residual RNA', cex=.75)
abline(h=1, lty=3)
points(x=mouse_screen$x, y=pmin(mouse_screen$resid,max(ylims)), col=alpha(mouse_screen$color, ci_alpha), pch=19)
barwidth = 0.5
rect(xleft=mouse_screen_smry$x - barwidth/2, xright=mouse_screen_smry$x + barwidth/2, ybottom=rep(0, nrow(mouse_screen_smry)), ytop=mouse_screen_smry$mean, col=alpha(mouse_screen_smry$color, ci_alpha), border=NA)
arrows(x0=mouse_screen_smry$x, y0=mouse_screen_smry$l95, y1=mouse_screen_smry$u95, code=3, angle=90, length=0.05, lwd=1.5)
mtext(LETTERS[panel], side=3, cex=2, adj = 0, line = 0.5)
panel = panel + 1



cellulo %>%
  filter(purpose=='IC50' & cells=='N2a') %>%
  group_by(descno, concnm, biorep) %>% # group first across techreps within biorep
  summarize(.groups='keep',
            resid = mean(resid)) %>%
  ungroup() %>%
  group_by(descno, concnm) %>%
  summarize(.groups = 'keep',
            n = n(),
            mean = mean(resid), 
            l95  = lower(resid),
            u95  = upper(resid)) %>%
  ungroup() %>%
  filter(!is.na(descno)) -> mouse_ic50

mouse_ic50 %>%
  distinct(descno) -> ic50s

ylims = c(0, 1.25)
xlims = range(mouse_ic50$concnm) * c(1/3.16, 3.16)

for (i in 1:nrow(ic50s)) {
  cellulo %>%
    filter(descno %in% ic50s$descno[i] & purpose=='IC50' & cells=='N2a') %>%
    group_by(descno, concnm, biorep) %>% # group first across techreps within biorep
    summarize(.groups='keep',
              resid = mean(resid)) %>%
    ungroup() %>%
    select(descno, concnm, resid) -> ic50_points
  ic50_points %>%
    group_by(descno, concnm) %>%
    summarize(.groups = 'keep',
              n = n(),
              mean = mean(resid), 
              l95  = lower(resid),
              u95  = upper(resid)) %>%
    ungroup() -> ic50_smry

par(mar=c(3,3,3,1))
plot(NA, NA, xlim=xlims, ylim=ylims, xaxs='i', yaxs='i', ann=F, axes=F, log='x')
axis(side=1, at=xlims, lwd.ticks=0, labels=NA)
axis(side=1, at=10^(-3:3), labels=NA, tck=-0.025)
axis(side=1, at=10^(-3:3), labels=-3:3, lwd.ticks=0, lwd=0, line=-1.0, cex.axis=.7)
mtext(side=1, line=1, text='log10(nM)', cex=0.75)
axis(side=2, at=0:4/2, labels=NA, tck=-0.025)
axis(side=2, at=0:4/2, labels=percent(0:4/2), lwd=0, line=-.75, las=2, cex.axis=.75)
mtext(side=2, line=1.75, text='residual RNA', cex=.75)
abline(h=c(0.5,1), lty=3, lwd=0.5)
points(x=ic50_points$concnm, y=ic50_points$resid, pch=20, cex=0.5)
m = drm(resid ~ concnm, fct=LL.4(), data=ic50_points)
plot(m, type='none', add=T)
mtext(side=3, line=1, text=ic50_smry$descno[1], cex=0.4)
mtext(side=3, line=0, text=paste0(formatC(m$coefficients['e:(Intercept)'],format='f',digits=1),' nM'), cex=0.4)
mtext(LETTERS[panel], side=3, cex=2, adj = -0.5, line = 0.5)
panel = panel + 1
}


write_supp_table(mouse_ic50, 'Mouse siRNA IC50 determination in N2a cells with qPCR readout.')


silence_is_golden = dev.off()
}




## Figure 2. Mouse POC #### 
tell_user('done.\nCreating Figure 2...')
if ('figure-2'=='figure-2') {

resx=300
png('display_items/figure-2.png',width=6.5*resx,height=7*resx,res=resx)

layout_matrix = matrix(c(1, 1, 2, 3,
                         4, 4, 4, 4,
                         5, 5, 6, 6,
                         7, 7, 8, 8), nrow=4, byrow=T)
layout(layout_matrix, heights=c(1,0.2,1,1))
panel = 1

### Mouse TE in vivo screen #### 

mouse_te = c('CMR-1403','CMR-1656')

proc = process_elisas(mouse_te, control_group='saline') %>%
  filter(dose_dio %in% c(0,10)) %>%
  arrange(dose_dio, tx) %>%
  inner_join(meta, by='tx') -> proc

proc %>%
  distinct(tx, display_tx, is_reference, is_ntc, sequence_number, color) %>%
  mutate(is_exna = grepl('exNA',tx,ignore.case=T)) %>%
  arrange(!is_reference, !is_ntc, sequence_number, is_exna) %>%
  mutate(x = row_number()) -> xes

proc$x = xes$x[match(proc$tx, xes$tx)]

par(mar=c(4,4,3,1))
xlims = range(proc$x)+c(-0.5,0.5)
ylims = c(0,1.55)
yats=0:6/4
ybigs=0:3/2
ybiglabs=percent(ybigs)
plot(NA, NA, xlim=xlims, ylim=ylims, axes=F, ann=F, xaxs='i', yaxs='i')
axis(side=1, at=xlims, labels=NA, lwd.ticks=0)
par(xpd=T)
text(x=xes$x, y=rep(-0.05, nrow(xes)), labels=xes$display_tx, srt=45, adj=1)
par(xpd=F)
axis(side=2, at=yats, tck = -0.02, labels=NA)
axis(side=2, at=ybigs, tck = -0.05, labels=NA)
axis(side=2, at=ybigs, lwd=0, labels=ybiglabs, las=2, line=-0.5)
mtext(side=2, line=2.75, text='residual PrP', cex=0.8)
proc %>%
  group_by(x, tx, display_tx, color) %>%
  summarize(.groups = 'keep',
            n = n(),
            mean = mean(rel),
            l95 = lower(rel),
            u95 = upper(rel),
            max = max(rel)) %>%
  ungroup() -> proc_smry
barwidth=0.6
rect(xleft=proc_smry$x-barwidth/2, xright=proc_smry$x+barwidth/2, ybottom=rep(0, nrow(proc_smry)), ytop=proc_smry$mean, col=alpha(proc_smry$color, ci_alpha), border=NA)
arrows(x0=proc_smry$x, y0=proc_smry$l95, y1=proc_smry$u95, col=proc_smry$color, lwd=1.5, code=3, angle=90, length=0.05)
abline(h=1, lty=3)
points(x=proc$x, y=proc$rel, col=proc$color, bg='#FFFFFF', pch=21)
text(x=proc_smry$x, y=proc_smry$max, pos=3, labels=percent(proc_smry$mean,digits=1), cex=0.7)
mtext(LETTERS[panel], side=3, cex=1, adj = -0.1, line = 0.5); panel = panel + 1

write_supp_table(proc, 'Mouse siRNA in vivo target engagement - individual animal data.')
write_supp_table(proc_smry, 'Mouse siRNA in vivo target engagement - summarized data.')

yb_wts %>%
  arrange(dpi) %>%
  group_by(animal) %>%
  slice(1) %>%
  select(animal, baseline_weight=weight) -> baselines


for (this_wt_dpi in c(71,125)) {
  
  
  wt_thisdpi_meta = tibble(inoculum=c('none','RML'),
                           x = c(1,2),
                           color = c('#A9A9A9','#8E2323'))
  
yb_wts %>%
  inner_join(baselines, by='animal') %>%
  mutate(rel=weight/baseline_weight-1) %>%
  inner_join(yb_blind, by='animal') %>%
  inner_join(yb_meta, by=c('cohort','inoculum')) %>%
  filter(this_wt_dpi == 71 | !grepl('75',cohort)) %>% # for 71 dpi we can include all RML-inoc'ed animals, while for 126, we must exclude those that received treatment at 75 dpi
  filter(dpi == this_wt_dpi) %>%
  inner_join(wt_thisdpi_meta, by='inoculum', suffix = c('_cohort','_inoc')) -> indiv_thisdpi_weights_rel


indiv_thisdpi_weights_rel %>%
  select(animal, dpi, weight, baseline_weight, rel, inoculum) -> indiv_weights_out
write_supp_table(indiv_weights_out, paste0('Weight changes at ',this_wt_dpi,' dpi in prion-infected vs. uninoculated mice - individual animals.'))

indiv_thisdpi_weights_rel %>%
  group_by(inoculum, x, color_inoc) %>%
  summarize(.groups='keep', 
            mean = mean(rel),
            sd = sd(rel),
            n = n(),
            l95 = mean(rel) - 1.96*sd(rel)/sqrt(n()),
            u95 = mean(rel) + 1.96*sd(rel)/sqrt(n())) %>%
  ungroup() -> wt_thisdpi_smry

uninoc = indiv_thisdpi_weights_rel$rel[indiv_thisdpi_weights_rel$inoculum=='none']
rml = indiv_thisdpi_weights_rel$rel[indiv_thisdpi_weights_rel$inoculum=='RML']

t_test_result = t.test(uninoc,rml)

wt_thisdpi_smry %>%
  select(-x, -color_inoc) %>%
  mutate(pval_ttest = case_when(inoculum=='none' ~ 1.0,
                                inoculum=='RML' ~ t_test_result$p.value)) -> wt_thisdpi_smry_out
write_supp_table(wt_thisdpi_smry_out, paste0('Weight changes at ',this_wt_dpi,' dpi in prion-infected vs. uninoculated mice - summarized.'))



indiv_thisdpi_weights_rel -> indiv_weights_rel
wt_thisdpi_smry -> wt_smry
par(mar=c(1.5,4,3,1))
xlims = c(0.5,2.5)
ylims = c(-0.10,0.30)
plot(NA, NA, xlim=xlims, ylim=ylims, xaxs='i', yaxs='i', ann=F, axes=F)
axis(side=1, at=xlims, labels=NA, lwd=1, lwd.ticks=0)
mtext(side=1, line=0.25, at=wt_smry$x, text=wt_smry$inoculum, cex=0.8)
axis(side=2, at=seq(-.3,.4,.1), labels=NA, las=2, tck=-0.05)
axis(side=2, at=seq(-.3,.4,.1), labels=percent(seq(-.3,.4,.1), signed=T), lwd=0, las=2, line=-0.5)
axis(side=2, at=seq(-.3,.4,.05), labels=NA, las=2, tck=-0.025)
abline(h=0, lty=3, lwd=1)
set.seed(1)
points(x=jitter(indiv_thisdpi_weights_rel$x, amount=.25), y=indiv_thisdpi_weights_rel$rel, pch=20, col=alpha(indiv_thisdpi_weights_rel$color_inoc, ci_alpha))
barwidth=0.3
segments(x0=wt_smry$x-barwidth, x1=wt_smry$x+barwidth, y0=wt_smry$mean, col=wt_smry$color_inoc, lwd=1.5)
arrows(x0=wt_smry$x, y0=wt_smry$l95, y1=wt_smry$u95, code=3, angle=90, length=0.05, col=wt_smry$color_inoc, lwd=1.5)
mtext(side=2, line=2.5, text='weight change', cex=0.8)
mtext(side=3, line=0, text=paste0(this_wt_dpi,' dpi'), cex=0.8)
axis(side=3, line=-3, at=wt_thisdpi_meta$x, tck=0.05, labels=NA)
mtext(side=3, line=-2.7, at=mean(wt_thisdpi_meta$x), text=paste0("P = ",formatC(t_test_result$p.value, format='fg', digits=2)), cex=0.7)
mtext(LETTERS[panel], side=3, cex=1, adj = -0.1, line = 0.5); panel = panel + 1
}



# 
# yb_wts %>%
#   inner_join(baselines, by='animal') %>%
#   mutate(rel=weight/baseline_weight-1) %>%
#   inner_join(yb_blind, by='animal') %>%
#   inner_join(yb_meta, by=c('cohort','inoculum')) %>%
#   filter(!grepl('75',cohort)) %>%
#   filter(dpi == 125) -> indiv_125_weights_rel
# 
# wt_125_meta = tibble(inoculum=c('none','RML'),
#                      x = c(1,2),
#                      color = c('#A9A9A9','#8E2323'))
# 
# indiv_125_weights_rel %>%
#   group_by(inoculum) %>%
#   summarize(.groups='keep', 
#             mean = mean(rel),
#             sd = sd(rel),
#             n = n(),
#             l95 = mean(rel) - 1.96*sd(rel)/sqrt(n()),
#             u95 = mean(rel) + 1.96*sd(rel)/sqrt(n())) %>%
#   ungroup() %>%
#   inner_join(wt_125_meta, by='inoculum') -> wt_125_smry
# 
# uninoc = indiv_125_weights_rel$rel[indiv_125_weights_rel$inoculum=='none']
# rml = indiv_125_weights_rel$rel[indiv_125_weights_rel$inoculum=='RML']
# 
# t.test(uninoc,rml)
# 
# indiv_125_weights_rel -> indiv_weights_rel
# wt_125_smry -> wt_smry
# par(mar=c(1.5,4,3,1))
# xlims = c(0.5,2.5)
# ylims = c(-0.10,0.30)
# plot(NA, NA, xlim=xlims, ylim=ylims, xaxs='i', yaxs='i', ann=F, axes=F)
# axis(side=1, at=xlims, labels=NA, lwd=1, lwd.ticks=0)
# mtext(side=1, line=0.25, at=wt_smry$x, text=wt_smry$inoculum)
# axis(side=2, at=seq(-.3,.4,.1), labels=NA, las=2, tck=-0.05)
# axis(side=2, at=seq(-.3,.4,.1), labels=percent(seq(-.3,.4,.1), signed=T), lwd=0, las=2, line=-0.5)
# axis(side=2, at=seq(-.3,.4,.05), labels=NA, las=2, tck=-0.025)
# abline(h=0, lty=3, lwd=1)
# set.seed(1)
# points(x=jitter(rep(1,length(uninoc)),amount=.25), y=uninoc, pch=20, col=alpha(uninoc_col, ci_alpha))
# points(x=jitter(rep(2,length(rml)),amount=.25), y=rml, pch=20, col=alpha(rml_col, ci_alpha))
# barwidth=0.3
# segments(x0=wt_smry$x-barwidth, x1=wt_smry$x+barwidth, y0=wt_smry$mean, col=wt_smry$color, lwd=1.5)
# arrows(x0=wt_smry$x, y0=wt_smry$l95, y1=wt_smry$u95, code=3, angle=90, length=0.05, col=wt_smry$color, lwd=1.5)
# mtext(side=2, line=2.5, text='weight change', cex=0.8)
# mtext(side=3, line=0, text='125 dpi', cex=0.8)
# mtext(LETTERS[panel], side=3, cex=2, adj = 0, line = 0.5)
# panel = panel + 1

par(mar=c(0,0,0,0))
plot(NA, NA, xlim=c(0,1), ylim=c(0,1), axes=F, ann=F, xaxs='i', yaxs='i')
yb_meta %>% distinct(color, lty, legdisp) -> leg
legend('top', horiz=T, bty='n', cex=0.9, lwd=2, leg$legdisp, col=leg$color, text.col=leg$color, lty=leg$lty)

yb_master %>% 
  inner_join(yb_blind, by='animal') %>%
  inner_join(yb_meta, by='cohort') %>%
  select(animal, cohort, dpi, acm, prion_endpoint, color, lty, inoculation_date) -> survdata

survdata %>%
  select(-color, -lty, -inoculation_date) -> survdata_out
write_supp_table(survdata_out, 'Survival in challenge study - individual animal data.')

survdata %>% 
  mutate(intervention_dpi = gsub('.*-','',cohort)) %>%
  mutate(tx = gsub('.*-','',gsub('-[0-9]*$','',cohort)) ) %>%
  filter(acm & tx != 'uninoc') %>% 
  group_by(cohort, intervention_dpi, tx) %>% 
  summarize(.groups='keep', 
            n=n(), 
            mean_dpi=mean(dpi),
            sd_dpi = sd(dpi),
            median_dpi = median(dpi)) %>%
  ungroup() %>%
  arrange(desc(intervention_dpi), desc(tx)) -> yb_survdata_smry

write_supp_table(yb_survdata_smry, 'Survival in challenge study - summarized.')

survdiff(Surv(dpi, acm) ~ cohort, data=subset(survdata, grepl('75',cohort)))
survdiff(Surv(dpi, acm) ~ cohort, data=subset(survdata, grepl('126',cohort)))

sf = survfit(Surv(dpi, acm) ~ cohort, data=survdata)
sf$color = yb_meta$color[match(gsub('cohort=','',names(sf$strata)), yb_meta$cohort)]
sf$lty = yb_meta$lty[match(gsub('cohort=','',names(sf$strata)), yb_meta$cohort)]

allsurvdata = survdata

for (this_txdpi in c(75,126)) {
  
  txdpi_marks = this_txdpi
  if (this_txdpi == 75) {
    txdpi_marks = c(75, 194, 315, 439)
  }
  
  allsurvdata %>% 
    filter(grepl(this_txdpi,cohort) | grepl('uninoc',cohort)) -> survdata
  
  sf = survfit(Surv(dpi, acm) ~ cohort, data=survdata)
  sf$color = yb_meta$color[match(gsub('cohort=','',names(sf$strata)), yb_meta$cohort)]
  sf$lty = yb_meta$lty[match(gsub('cohort=','',names(sf$strata)), yb_meta$cohort)]
  
  par(mar=c(3,4,1,1))
  xlims = c(0,500)
  ylims = c(0,1.05)
  plot(sf, col=sf$color, lwd=2, lty=sf$lty, xlim=xlims, ylim=ylims, axes=F, ann=F, xaxs='i', yaxs='i')
  axis(side=1, at=seq(0,600,100), labels=NA, tck=-0.05)
  axis(side=1, at=seq(0,600,100), lwd=0, labels=seq(0,600,100), line=-0.5)
  axis(side=1, at=seq(0,600,10), labels=NA, tck=-0.025)
  mtext(side=1, line=1.5, text='days post-inoculation')
  axis(side=2, at=0:4/4, labels=percent(0:4/4), las=2)
  mtext(side=2, line=2.75, text='survival', cex=0.8)
  par(xpd=T)
  points(x=c(txdpi_marks), y=rep(max(ylims),length(txdpi_marks)), pch=25, bg='#000000')
  par(xpd=F)
  mtext(LETTERS[panel], side=3, cex=1, adj = -0.1, line = 0.5); panel = panel + 1
  

  yb_wts %>%
    inner_join(yb_blind %>% select(-inoculum), by='animal') %>%
    inner_join(yb_meta, by='cohort') %>%
    filter(txdpi == this_txdpi) -> weights_curr

  weights_curr %>%
    arrange(dpi) %>%
    group_by(animal) %>%
    slice(1) %>%
    select(animal, baseline_weight=weight) -> baselines
  
  
  
  weights_curr %>%
    inner_join(baselines, by='animal') %>%
    mutate(rel=weight/baseline_weight-1) %>%
    group_by(cohort, dpi, color, lty) %>%
    summarize(.groups='keep',
              mean = mean(rel),
              sd = sd(rel),
              n = n(),
              l95 = mean(rel) - 1.96*sd(rel)/sqrt(n()),
              u95 = mean(rel) + 1.96*sd(rel)/sqrt(n())) %>%
    ungroup() %>%
    filter(n > 2) -> weights_rel
  
  
  weights_rel %>% filter(grepl(this_txdpi, cohort) | grepl('uninoc',cohort)) -> weights_curr
  
  weights_rel %>%
    select(-color, -lty) -> weights_rel_out
  write_supp_table(weights_rel_out, paste0('Weight changes in challenge study ',this_txdpi,' dpi - individual animal data.'))
  weights_curr %>%
    select(-color, -lty) -> weights_curr_out
  write_supp_table(weights_curr_out, paste0('Weight changes in challenge study ',this_txdpi,' dpi - summarized.'))
  
  par(mar=c(3,4,1,4))
  xlims = c(0, 500)
  ylims = c(-.3, .3)
  plot(NA, NA, xlim=xlims, ylim=ylims, axes=F, ann=F, xaxs='i', yaxs='i')
  axis(side=1, at=seq(0,600,100), labels=NA, tck=-0.05)
  axis(side=1, at=seq(0,600,100), lwd=0, labels=seq(0,600,100), line=-0.5)
  axis(side=1, at=seq(0,600,10), labels=NA, tck=-0.025)
  mtext(side=1, line=1.5, text='days post-inoculation')
  axis(side=2, at=seq(-.3,.4,.1), labels=NA, las=2, tck=-0.05)
  axis(side=2, at=seq(-.3,.4,.1), labels=percent(seq(-.3,.4,.1), signed=T), lwd=0, las=2, line=-0.5)
  axis(side=2, at=seq(-.3,.4,.05), labels=NA, las=2, tck=-0.025)
  abline(h=0)
  abline(h=-.2, col='red', lty=3)
  mtext(side=4, at=-.2, las=2, col='red', text='endpoint', cex=.8)
  mtext(side=2, line=2.75, text='weight change', cex=0.8)
  for (coh in unique(weights_curr$cohort)) {
    subs = weights_curr[weights_curr$cohort==coh,]
    points(subs$dpi, subs$mean, type='l', lwd=2, col=subs$color, lty=subs$lty)
    polygon(x=c(subs$dpi,rev(subs$dpi)),y=c(subs$l95,rev(subs$u95)),col=alpha(subs$color,ci_alpha),border=NA)
  }
  par(xpd=T)
  points(x=c(txdpi_marks), y=rep(max(ylims),length(txdpi_marks)), pch=25, bg='#000000')
  par(xpd=F)
  mtext(LETTERS[panel], side=3, cex=1, adj = -0.1, line = 0.5); panel = panel + 1
  
}

silence_is_golden = dev.off()
}




## Table 1. Humanized mouse models #### 
tell_user('done.\nCreating Table 1...')
elisa_hu = rbind_files('data/elisa/','02[67]_summary.tsv') %>%
  filter(!grepl('QC|Neg',sample)) %>%
  rename(animal=sample) %>%
  mutate(plate = as.integer(substr(file,1,3)))
samples = read_tsv('data/humanized/mouse_genotypes.tsv', col_types=cols()) %>%
  mutate(animal = as.character(id))
params = read_tsv('data/humanized/mouse_genotypes_params.tsv', col_types=cols()) %>%
  mutate(y = max(row_number()) - row_number())

elisa_hu %>%
  inner_join(samples, by=c('animal')) %>%
  group_by(plate) %>%
  mutate(rel = ngml_av / mean(ngml_av[gt=='wild-type'])) %>%
  ungroup() %>%
  select(animal, gt, sex, age_wks, rel, ngml_av, plate) %>%
  inner_join(params, by='gt') -> mashup

mashup %>%
  rename(genotype=disp) %>%
  select(genotype, color, animal, sex, age_wks, plate, ngml_av, rel) -> mashup_out
write_supp_table(mashup_out, 'PrP expression in human PrP transgenic mouse lines - individual animal data.')


# check there is no age effect (at least within the narrow age range included here)
m = lm(rel ~ age_wks + gt + sex, data=mashup)
# summary(m)

smry = mashup %>%
  group_by(y, disp, color, copy_number) %>%
  summarize(.groups='keep',
            mean_raw = mean(ngml_av),
            mean = mean(rel),
            l95 = lower(rel),
            u95 = upper(rel),
            n = n(),
            mf = paste0(sum(sex=='M'),'M/',sum(sex=='F'),'F')) %>%
  ungroup()

write_supp_table(smry, 'PrP expression in human PrP transgenic mouse lines - summarized.')

## Figure 3 humanized mice #### 
if ('figure-3'=='figure-3') {
  tell_user('done.\nCreating Figure 3...')
resx=300
png('display_items/figure-3.png', width=6.5*resx, height=5*resx, res=resx)

layout_matrix = matrix(c(1,1,2,
                         3,3,3,
                         4,4,4), nrow=3, byrow=T)
layout(layout_matrix, heights=c(1,.8,.2))

panel = 1

### PrP expression by ELISA #### 
par(mar=c(4,8,3,1))
xlims = c(0, 6.7)
ylims = range(smry$y) + c(-0.5, 0.5)
barwidth=0.33
dilution = 200
llq = 0.05
meanwt = smry$mean_raw[smry$disp=='WT']
llq_relative = llq * dilution / meanwt
plot(NA, NA, xlim=xlims, ylim=ylims, xaxs='i', yaxs='i', axes=F, ann=F)
axis(side=1, at=0:6, tck=-0.05, labels=NA)
axis(side=1, at=0:60/10, tck=-0.02, labels=NA)
axis(side=1, at=0:6, line=-0.5, lwd=0)
mtext(side=1, line=1.5, text='brain PrP (fold WT)', cex=0.8)
axis(side=2, at=ylims, labels=NA, lwd.ticks=0)
mtext(side=2, at=smry$y, text=smry$disp, col='#000000', las=2, line=0.25,cex=0.8)
abline(v=llq_relative, lwd=0.75, lty=3, col='red')
abline(v=1,lty=3)
mtext(side=3, at=llq_relative, cex=0.75, font=2, line=0.25, text='   LLQ', col='red')
arrows(x0=smry$l95, x1=smry$u95, y0=smry$y, angle=90, length=0.05, code=3, lwd=1, col=smry$color)
rect(xleft=rep(0 ,nrow(smry)), xright=smry$mean, ybottom=smry$y-barwidth, ytop=smry$y+barwidth, lwd=1, col=alpha(smry$color,ci_alpha), border=NA)
points(x=mashup$rel, y=mashup$y, pch=21, col=mashup$color, bg='#FFFFFF')
mtext(LETTERS[panel], side=3, cex=1, adj = -0.3, line = 0.0); panel = panel + 1

### copy number vs. ELISA #### 
par(mar=c(4,4,3,1))
plot(smry$copy_number, smry$mean, xlim=c(0,22), ylim=c(0,11),
     col=smry$color, xaxs='i', yaxs='i', pch=19, axes=F,
     ann=F)
mtext(side=1, line=1.6, text='DNA copy number')
mtext(side=2, line=1.7, text='PrP expression (fold WT)')
abline(a=0, b=0.5, lty=3, lwd=0.5)
axis(side=1, at=0:22, tck=-0.02, labels=NA)
axis(side=1, at=0:4*5, cex.axis=0.8, lwd=0, line=-0.5)
axis(side=1, at=0:4*5, tck=-0.05, labels=NA)
axis(side=2, at=0:11, tck=-0.02, labels=NA)
axis(side=2, at=0:2*5, lwd=0, line=-0.25, las=2, cex.axis=0.8)
axis(side=2, at=0:2*5, labels=NA, tck=-0.05)
mtext(LETTERS[panel], side=3, cex=1, adj = -0.1, line = 0.0); panel = panel + 1

### BAC coverage #### 
# 
# layout_matrix = matrix(1:2, nrow=2, byrow=T)
# layout(layout_matrix, heights=c(1,.25))


# twist targets = c(4570000, 4710000)
# relevant region for BAC appears to be: 4640000, 4710000
# original BAM was aligned to GRCh37, need to offset to GRCh38
grch38_coding_offset = 4699605 - 4680251
depth = read_tsv('data/humanized/CYAGEN307331_depth.tsv', col_types=cols()) %>%
  mutate(pos = pos + grch38_coding_offset)
all_possible_pos = tibble(pos=seq(4570000,4740000),by=1)


depth %>%
  select(-chrom) %>%
  right_join(all_possible_pos, by='pos') %>%
  mutate(depth=replace_na(depth,0)) %>%
  arrange(pos) -> depthtbl

depthtbl %>%
  mutate(pos100 = floor(pos/100)*100) %>%
  group_by(pos100) %>%
  summarize(.groups='keep',
            p30 = mean(depth >= 30),
            p10 = mean(depth >= 10),
            mn = mean(depth)) %>%
  ungroup() -> depth100

depth100 %>%
  rename(chr20_position_rounded_to_nearest_100 = pos100,
         proportion_at_30x_depth_or_higher = p30,
         proportion_at_10x_depth_or_higher = p10,
         mean_depth = mn) -> depth100_out
write_supp_table(depth100_out, 'Sequencing depth by 100 bp chromosomal interval in Tg26372 mouse.')


#xlims = c(4640000, 4710000)
xlims = c(4570000, 4740000)
pseudozero = 1
ylims = c(pseudozero,1e5)
ybigs = c(1, 10, 100, 1000, 10000)
yats = rep(1:9, 6) * 10^(rep(-1:4, each=9))
ybiglabs = c('≤1', '10', '100', '1K','10K')
xbigs = seq(min(xlims), max(xlims), 10000)
xats = seq(min(xlims), max(xlims), 1000)

par(mar=c(1,4,1,2))
plot(NA, NA, xlim=xlims, ylim=ylims, axes=F, ann=F, xaxs='i', yaxs='i', log='y')
axis(side=1, at=xbigs, labels=NA,tck=-0.05)
axis(side=1, at=xats, labels=NA,tck=-0.02)
axis(side=1, at=xbigs, line=-0.5, labels=paste0(formatC(xbigs/1e6,digits=2,format='f'),'M'), lwd=0)
axis(side=2, at=ybigs, labels=NA,tck=-0.05)
axis(side=2, at=yats, labels=NA,tck=-0.02)
axis(side=2, at=ybigs, line=-0.25, las=2, labels=ybiglabs, lwd=0)
mtext(side=2, line=2.5, text='sequencing depth', cex=0.8)
mtext(side=1, line=1.6, adj=0, text='chr20 position', cex=0.8)
polygon(c(depth100$pos100,max(depth100$pos100),rev(depth100$pos100)), c(pmax(depth100$mn,pseudozero),pseudozero,rep(0,nrow(depth100))), lwd=3, col='#CEAB1277', border='#CEAB12')
mtext(LETTERS[panel], side=3, cex=1, adj = -0.05, line = 0.0); panel = panel + 1

exon1_start = c(4686456,4721909 )# transcription start site
exon1_end = c(4686512,4721969)
exon2_start =c(4699211,4724541)
exon2_end = c(4701588,4728460) # transcription end site
cds_start = c(4699221,4724552)
cds_end = c(4699982,4725079)

landmarks = tibble(gene = c('PRNP','PRND'),
                   exon1_start = exon1_start,
                   exon2_start = exon2_start,
                   exon1_end   = exon1_end,
                   exon2_end   = exon2_end,
                   cds_start   = cds_start,
                   cds_end     = cds_end,
                   exon2_fill  = '#A3A3A3',
                   y = 2)

intron_lwd = 1
utr_lwd = 10
cds_lwd = 20

default_fill = '#000000'
utr_height = 2
cds_height = 4
prnd_height = 1

par(mar=c(0,4,0,2))
ylims = c(-2, 6)
plot(NA, NA, xlim=xlims, ylim=ylims, axes=F, ann=F, xaxs='i', yaxs='i')
segments(x0=landmarks$exon1_start, x1=landmarks$exon2_end, y0=landmarks$y, lwd=intron_lwd, lend=1)
rect(xleft=landmarks$exon1_start, xright=landmarks$exon1_end, ybottom=landmarks$y-utr_height/2, ytop=landmarks$y+utr_height/2, col=default_fill, border=NA)
rect(xleft=landmarks$exon2_start, xright=landmarks$exon2_end, ybottom=landmarks$y-utr_height/2, ytop=landmarks$y+utr_height/2, col=default_fill, border=NA)
rect(xleft=landmarks$cds_start, xright=landmarks$cds_end, ybottom=landmarks$y-cds_height/2, ytop=landmarks$y+cds_height/2, col=default_fill, border=NA)
text(x=(landmarks$exon2_end + landmarks$exon1_start)/2, y=landmarks$y-1.4, pos=1, labels=landmarks$gene, font=3, cex=0.8)
#text(x=(landmarks$exon2_end + landmarks$exon1_start)/2, y=landmarks$y, pos=1, labels=landmarks$gene, font=3, cex=0.8)

silence_is_golden = dev.off() ### end Fig 3 humanized mice #### 
}

tla_primers = read_tsv('data/supptables/tla_primers.tsv', col_types=cols())
write_supp_table(tla_primers, 'PCR primers used for TLA transgene mapping analysis.')


depthtbl %>% 
  filter(depth > 100) %>% 
  summarize(minpos = min(pos), maxpos=max(pos)) -> bacrange
# upstream distance
upstream_distance = bacrange$minpos - landmarks$exon1_start[1]
# downstream distance
downstream_distance = bacrange$maxpos - landmarks$exon2_end[1]

bacrange_out = tibble(property=c('span upstream of TSS','span downstream of TES'),
                      distance_bases = c(upstream_distance, downstream_distance))
write_supp_table(bacrange_out, 'Span of human BAC in transgenic mice relative to PRNP transcript.')





## Figure 4. Human TE screen #### 

# layout_matrix = matrix(c(1,2,
#                          3,3), nrow=2, byrow=T)
# layout(layout_matrix, widths=c(1.25, 1))

cellulo %>%
  filter(purpose=='IC50' & cells=='U251-MG') %>%
  filter(!is.na(descno)) %>%
  group_by(source_well, biorep, descno, concnm) %>%
  summarize(.groups='keep',
            n_techreps = n(),
            mean_resid = mean(resid)) %>%
  ungroup() -> human_ic50

human_ic50 %>%
  distinct(descno) -> human_ic50_descnos

screen_meta = tibble(status=c('advanced','not advanced'),
                     color = c('#DD4400','#7979A9'))

cellulo %>%
  filter(purpose=='screen' & cells=='U251-MG') %>%
  filter(source_well != 'no') %>%
  mutate(x = rank_uniq(descno)) %>%
  mutate(x = case_when(source_well=='untreated' ~ 0,
                   TRUE ~ x)) %>%
  mutate(y = max(x) - x + 1) %>%
  group_by(x, y, source_well, biorep, descno, concnm) %>%
  summarize(.groups='keep',
            n_techreps = n(),
            mean_resid = mean(resid)) %>%
  ungroup() %>%
  mutate(status = case_when(descno %in% human_ic50_descnos$descno ~ 'advanced',
                            TRUE ~ 'not advanced')) %>%
  inner_join(screen_meta, by='status') -> bioreps

bioreps %>%
  group_by(x, y, descno, concnm, status, color) %>%
  summarize(.groups='keep',
            n_bioreps = n(),
            mean = mean(mean_resid),
            l95 = lower(mean_resid),
            u95 = upper(mean_resid)) %>%
  ungroup() %>%
  mutate(disp = case_when(is.na(descno) ~ 'UNT',
                          TRUE ~ as.character(descno))) -> cellulo_smry

cellulo_smry %>%
  rename(sequence=disp) %>%
  select(sequence, concnm, status, n_bioreps, mean, l95, u95) -> cellulo_smry_out
write_supp_table(cellulo_smry_out, 'Human siRNA sequences screened in U251-MG cells with qPCR readout.')



cellulo_smry %>%
  filter(concnm==2000) %>%
  filter(mean < 0.1) %>%
  nrow() -> n_10pctresid_2000nm

cellulo_smry %>%
  filter(concnm==500) %>%
  filter(mean < 0.1) %>%
  nrow() -> n_10pctresid_500nm

# 
# 
# for (this_conc in c(500, 2000)) {
#   bioreps %>% filter(concnm==this_conc) -> bioreps_this
#   cellulo_smry %>% filter(concnm==this_conc) -> cellulo_this
#   if(this_conc==500) {
#     par(mar=c(3,3,3,0.5))
#   } else {
#     par(mar=c(3,0,3,0.5))
#   }
#   
#   ylims = range(cellulo_this$y) + c(-0.5, 0.5)
#   xlims = c(-0.05, 1.25)
#   xats = 0:13/10
#   xbigs = 0:2/2
#   plot(NA, NA, xlim=xlims, ylim=ylims, axes=F, ann=F, xaxs='i', yaxs='i')
#   axis(side=1, at=xats, tck=-0.02, labels=NA)
#   axis(side=1, at=xbigs, tck=-0.05, labels=NA)
#   axis(side=1, at=xbigs, lwd=0, line=-0.5, labels=percent(xbigs))
#   axis(side=2, at=ylims, labels=NA, lwd.ticks=0)
#   abline(v=1, lty=3)
#   if(this_conc==500) {
#     mtext(side=2, at=cellulo_this$y, text=cellulo_this$disp, col=cellulo_this$color, las=2, cex=0.5)
#   } 
#   barwidth=0.7
#   rect(xleft=rep(0,nrow(cellulo_this)), xright=cellulo_this$mean, ybottom=cellulo_this$y-barwidth/2, ytop=cellulo_this$y + barwidth/2, border=NA, col=alpha(cellulo_this$color, ci_alpha))
#   arrows(x0=cellulo_this$l95, x1=cellulo_this$u95, y0=cellulo_this$y, angle=90, code=3, length=0.02, col=cellulo_this$color)
#   points(x=bioreps_this$mean_resid, y=bioreps_this$y, col=bioreps_this$color, bg='#FFFFFF', pch=21, cex=0.5)
#   
# }


# idx = 2439
# # full target:
# rcompu(substr(huseq, idx, idx+20-1))
# 
# # actual sequence:
# paste0('U',dna_to_rna(rcompu(substr(huseq, idx+2, idx+20-1))),'UU')

# idx=1803
# paste0('U',dna_to_rna(rcomp(substr(huseq, idx+2, idx+20-1))),'UU')
# 
# 
# # for off-target search: 
# "GAATACTCACAAAGTGCA"
# rcomp("AATACTCACAAAGTGCA")
# "TGCACTTTGTGAGTATT"

### actual Figure 4 beginning #### 
if ('figure-4'=='figure-4') {
  tell_user('done.\nCreating Figure 4...')
resx=600
png('display_items/figure-4.png',width=6.5*resx,height=4.5*resx,res=resx)

layout_matrix = matrix(c(1,  1,  1,  1,  1,   4,  5,  6,  7,  8,
                         1,  1,  1,  1,  1,   9,  10, 11, 12, 13,
                         2,  2,  2,  2,  2,   14, 14, 14, 14, 14,
                         3,  3,  3,  3,  3,   14, 14, 14, 14, 14,
                         15, 15, 15, 15, 15,  15, 16, 16, 16,16), nrow=5, byrow=T)
layout(layout_matrix, heights = c(.5, .5, 1, .5, 1))
# layout.show(17)

panel = 1

### U251-MG screening #### 

for (this_conc in c(2000, 500)) {
  bioreps %>% filter(concnm==this_conc) -> bioreps_this
  cellulo_smry %>% filter(concnm==this_conc) -> cellulo_this
  if(this_conc==500) {
    par(mar=c(0.5,3,0.5,3))
  } else {
    par(mar=c(0.1,3,1,3))
  }
  
  xlims = range(cellulo_this$x) + c(-0.5, 0.5)
  ylims = c(0, 1.25)
  yats = 0:13/10
  ybigs = 0:2/2
  plot(NA, NA, xlim=xlims, ylim=ylims, axes=F, ann=F, xaxs='i', yaxs='i')
  axis(side=1, at=xlims, labels=NA, lwd.ticks=0)
  axis(side=2, at=yats, tck=-0.02, labels=NA)
  axis(side=2, at=ybigs, tck=-0.05, labels=NA)
  axis(side=2, at=ybigs, lwd=0, line=-0.75, labels=percent(ybigs), las=2,cex=0.8)
  axis(side=2, at=xlims, labels=NA, lwd.ticks=0)
  mtext(side=3, line=-1.25, adj=1, text=paste0(this_conc/1000,' µM'), cex=0.8)
  mtext(side=2, at=mean(ylims), text=expression(residual~italic(PRNP)), cex=0.7, line=2.1)
  abline(h=1, lty=3)
  if(this_conc==500) {
    mtext(side=1, at=cellulo_this$x, text=cellulo_this$disp, col=cellulo_this$color, las=2, cex=0.2, line=0.1)
  } 
  barwidth=0.7
  rect(ybottom=rep(0,nrow(cellulo_this)), ytop=cellulo_this$mean, xleft=cellulo_this$x-barwidth/2, xright=cellulo_this$x + barwidth/2, border=NA, col=alpha(cellulo_this$color, ci_alpha))
  arrows(y0=cellulo_this$l95, y1=cellulo_this$u95, x0=cellulo_this$x, angle=90, code=3, length=0.02, col=cellulo_this$color)
  points(y=bioreps_this$mean_resid, x=bioreps_this$x, col=bioreps_this$color, bg='#FFFFFF', pch=21, cex=0.5)
  
  if (this_conc==2000) { # only on the first iteration
    mtext(LETTERS[panel], side=3, cex=1, adj = -0.1, line = -0.3); panel = panel + 1
  }
}

### CDS diagram #### 

landmarks = tibble(gene = c('PRNP'),
                   tss = 362, # 
                   cds_start   = 429,
                   cds_end     =   429+762,
                   tes = 2808,
                   y=0.5)

intron_lwd = 1
utr_lwd = 10
cds_lwd = 20
default_fill = '#000000'
utr_height = 0.3
cds_height = 1

par(mar=c(1.5,3,0.2,3))
ylims = c(0, 2.4)
xlims = c(0, nchar(huseq))
plot(NA, NA, xlim=xlims, ylim=ylims, axes=F, ann=F, xaxs='i', yaxs='i')

minimum_y = 1.3
maximum_y = 1.8
top_xlims = range(cellulo_this$x) + c(-0.5, 0.5)
new_xlims = c(0, nchar(huseq))
cellulo_this$xpos = min(new_xlims) + diff(new_xlims) * ((cellulo_this$x - min(top_xlims)) / diff(top_xlims))
segments(x0=cellulo_this$descno, x1=cellulo_this$xpos, y0=rep(minimum_y, nrow(cellulo_this)), y1=rep(maximum_y,nrow(cellulo_this)), col=cellulo_this$color, lwd=0.5)
segments(x0=cellulo_this$descno, y0=rep(minimum_y, nrow(cellulo_this)), y1=rep(minimum_y, nrow(cellulo_this))-0.9, col=cellulo_this$color, lwd=0.5)
segments(x0=cellulo_this$xpos[cellulo_this$disp!='UNT'], y0=rep(maximum_y, nrow(cellulo_this)-1), y1=rep(max(ylims), nrow(cellulo_this)-1), col=cellulo_this$color[cellulo_this$disp!='UNT'], lwd=0.5)
mtext(side=2, at=landmarks$y, text='PRNP',line = 2, adj=0, font=3, las=2)
mtext(side=1, at=c(landmarks$tss-50, 
                   (landmarks$cds_start+ landmarks$cds_end) /2,
                   (landmarks$cds_end + landmarks$tes)/2),
      text=c("5'UTR","CDS","3'UTR"),line = 0.25,  font=1, cex=0.7)
rect(xleft=landmarks$tss, xright=landmarks$tes, ybottom=landmarks$y-utr_height/2, ytop=landmarks$y+utr_height/2, col=default_fill, border=NA)
rect(xleft=landmarks$cds_start, xright=landmarks$cds_end, ybottom=landmarks$y-cds_height/2, ytop=landmarks$y+cds_height/2, col=default_fill, border=NA)


### IC50s #### 

cellulo %>%
  filter(purpose=='IC50' & cells=='U251-MG' & !is.na(descno)) %>%
  distinct(descno, concnm) -> human_ic50

human_ic50 %>%
  distinct(descno) -> ic50s

ylims = c(0, 1.25)
xlims = range(human_ic50$concnm) * c(1/3.16, 3.16)
xbigs = 10^(-3:3)
xbiglabs = -3:3
xats = rep(1:9, 7) * rep(10^(-3:3), each=9)

cellulo %>%
  filter(!is.na(descno) & purpose=='IC50' & cells=='U251-MG') %>%
  select(descno, concnm, biorep, resid) %>%
  group_by(descno, concnm, biorep) %>% # group technical replicates & take mean
  summarize(.groups='keep', resid=mean(resid)) %>%
  ungroup() %>%
  group_by(descno, concnm) %>%
  summarize(.groups = 'keep',
            n_bioreps = n(), # we already grouped techreps, so this n is bioreps
            mean = mean(resid), 
            l95  = lower(resid),
            u95  = upper(resid)) %>%
  ungroup() -> ic50_smry_out

write_supp_table(ic50_smry_out, 'Human siRNA sequence IC50 determination in U251-MG cells by qPCR.')

for (i in 1:nrow(ic50s)) {
  cellulo %>%
    filter(descno %in% ic50s$descno[i] & !is.na(descno) & purpose=='IC50' & cells=='U251-MG') %>%
    select(descno, concnm, biorep, resid) %>%
    group_by(descno, concnm, biorep) %>% # group technical replicates & take mean
    summarize(.groups='keep', resid=mean(resid)) %>%
    ungroup() -> ic50_points
  ic50_points %>%
    group_by(descno, concnm) %>%
    summarize(.groups = 'keep',
              n_bioreps = n(), # we already grouped techreps, so this n is bioreps
              mean = mean(resid), 
              l95  = lower(resid),
              u95  = upper(resid)) %>%
    ungroup() -> ic50_smry
  
  if (i <= nrow(ic50s)/2) {
    par(mar=c(0.25,0,2,0))
  } else {
    par(mar=c(2,0,0.25,0))
  }
  plot(NA, NA, xlim=xlims, ylim=ylims, xaxs='i', yaxs='i', ann=F, axes=F, log='x')
  axis(side=1, at=xlims, lwd.ticks=0, labels=NA)
  axis(side=1, at=xbigs, labels=NA, tck=-0.05)
  axis(side=1, at=xats, labels=NA, tck=-0.02)
  axis(side=2, at=ylims, labels=NA, lwd.ticks=0)
  if (i==1) { # first panel only
    mtext(LETTERS[panel], side=3, cex=1, adj = -0.2, line = 0.25); panel = panel + 1
  }
  if (i %% 5 == 1) { # first in each row
    axis(side=2, at=0:4/2, labels=NA, tck=0.025)
    axis(side=2, at=0:4/2, labels=percent(0:4/2), lwd=0, line=-.75, las=2, cex.axis=.75)
    
  }
  if (i > nrow(ic50s)/2) { # bottom row
    axis(side=1, at=xbigs, labels=xbiglabs, lwd.ticks=0, lwd=0, line=-1.0, cex.axis=.7)
    mtext(side=1, line=1, text='log10(nM)', cex=0.6)
  }
  if (i==6) { # bottom left
    mtext(side=2, line=1.75, at=max(ylims), text=expression(residual~italic(PRNP)), cex=.8)
  }
  abline(h=c(0.5,1), lty=3, lwd=0.5)
  points(x=ic50_points$concnm, y=ic50_points$resid, pch=20, cex=0.5)
  m = drm(resid ~ concnm, fct=LL.4(), data=ic50_points)
  plot(m, type='none', add=T)
  mtext(side=1, line=-1.5, adj=0, text=paste0('  ',ic50_smry$descno[1]), cex=0.35)
  mtext(side=1, line=-1, adj=0, text=paste0('  ',formatC(m$coefficients['e:(Intercept)'],format='f',digits=1),' nM'), cex=0.35)
  #mtext(LETTERS[panel], side=3, cex=2, adj = -0.5, line = 0.5)
  #panel = panel + 1
}






human_te_screen = c('CMR-1313','CMR-1465','CMR-1697')

process_elisas(human_te_screen, control_group='saline') %>%
  filter(dose_dio %in% c(0,10)) %>%
  arrange(dose_dio, tx) %>%
  inner_join(meta, by='tx') -> proc

proc %>%
  distinct(tx, display_tx, is_reference, is_ntc, sequence_number, color) %>%
  mutate(is_exna = grepl('exNA',tx,ignore.case=T)) %>%
  arrange(!is_reference, !is_ntc, sequence_number, is_exna) %>%
  mutate(x = row_number()) -> xes

proc$x = xes$x[match(proc$tx, xes$tx)]
# 
# proc_smry = dviz(proc, xlims=range(proc$x)+c(-0.5,0.5), ylims=c(0,1.55),
#                  xvar='x', yvar='rel', xcols='tx', log='', colorvar = 'color',
#                  xats=proc$x, xbigs=proc$x, xbiglabs=NA, xlwds=0, yats=0:6/4, ybigs=0:2/2, ybiglabs=percent(0:2/2),
#                  xlab = 'compound', ylab='PrP (% saline)', barwidth=0.3, mar=c(5,4,1,1), xlabline=4, ylabline=3)
# mtext(side=1, at=xes$x, text=xes$tx, las=2, cex=0.8)
# abline(h=1, lty=3)

### in vivo screening #### 

par(mar=c(4,2,3,1))
xlims = range(proc$x)+c(-0.5,0.5)
ylims = c(0,1.55)
yats=0:6/4
ybigs=0:3/2
ybiglabs=percent(ybigs)
plot(NA, NA, xlim=xlims, ylim=ylims, axes=F, ann=F, xaxs='i', yaxs='i')
axis(side=1, at=xlims, labels=NA, lwd.ticks=0)
par(xpd=T)
text(x=xes$x, y=rep(-0.05, nrow(xes)), labels=xes$display_tx, srt=45, adj=1)
par(xpd=F)
axis(side=2, at=yats, tck = -0.02, labels=NA)
axis(side=2, at=ybigs, tck = -0.05, labels=NA)
axis(side=2, at=ybigs, lwd=0, labels=ybiglabs, las=2, line=-0.5)
mtext(side=2, line=2.75, text='residual PrP', cex=0.8)
proc %>%
  group_by(x, tx, display_tx, color) %>%
  summarize(.groups = 'keep',
            n = n(),
            mean = mean(rel),
            l95 = lower(rel),
            u95 = upper(rel),
            max = max(rel)) %>%
  ungroup() -> proc_smry
barwidth=0.6
rect(xleft=proc_smry$x-barwidth/2, xright=proc_smry$x+barwidth/2, ybottom=rep(0, nrow(proc_smry)), ytop=proc_smry$mean, col=alpha(proc_smry$color, ci_alpha), border=NA)
arrows(x0=proc_smry$x, y0=proc_smry$l95, y1=proc_smry$u95, col=proc_smry$color, lwd=1.5, code=3, angle=90, length=0.05)
abline(h=1, lty=3, lwd=0.5)
points(x=proc$x, y=proc$rel, col=proc$color, bg='#FFFFFF', pch=21)
par(xpd=T)
text(x=proc_smry$x, y=proc_smry$max+0.2, labels=percent(proc_smry$mean,digits=1), cex=0.7, srt=90)
par(xpd=F)
mtext(LETTERS[panel], side=3, cex=1, adj = -0.1, line = 0.5); panel = panel + 1

proc %>%
  select(study_id, plate, animal, genotype, sex, display_tx, dose_dio, dosing_regimen, days_harvest, ngml_av, saline_mean, rel) -> proc_out
write_supp_table(proc_out, 'Human siRNA sequences tested in vivo - individual animal data.')
proc_smry %>%
  select(display_tx, n, mean, l95, u95) -> proc_smry_out
write_supp_table(proc_smry_out, 'Human siRNA sequences tested in vivo - summarized.')

### Regional qPCR on top human candidates #### 

regional_qpcr_studies = c('CMR-1313', 'CMR-1697') # CMR-1313 is 2440 only (both hiPS and exNA). CMR-1697 includes 2439, 2520, and 2768

qpcr %>%
  filter(study_id %in% regional_qpcr_studies) %>%
  filter(target=='PRNP') %>%
  group_by(study_id, qpcr_id, region, biorep, sample_group) %>%
  summarize(.groups='keep',
            n_techreps = n(),
            mean_twoddct = mean(twoddct)) %>%
  ungroup() %>%
  group_by(study_id, qpcr_id, region) %>%
  mutate(residual = mean_twoddct / mean(mean_twoddct[sample_group %in% c('saline','PBS')])) %>%
  ungroup() -> regional_qpcr

tx_compared_by_qpcr = tibble(tx_on_qpcr_plate=c('2768','2520','2439','2440-exNA','saline','PBS'),
                             tx = c('2768-s4','2520-s4','2439-s4','2440-s4','saline','saline'),
                             x_offset = c(-0.15, 0.0, 0.3, 0.15, -0.3, -0.3))
tx_compared_by_qpcr$color = meta$color[match(tx_compared_by_qpcr$tx, meta$display_tx)]

region_meta = tibble(region=c("HP", "PFC", "VC", "Str", "Thal", "CB"),
                     x = 1:6)

regional_qpcr %>%
  inner_join(tx_compared_by_qpcr, by=c('sample_group'='tx_on_qpcr_plate')) %>%
  select(tx, region, residual, x_offset, color) %>%
  inner_join(region_meta, by='region') -> regional_indivs

regional_indivs %>% 
  group_by(tx, region, x, x_offset, color) %>%
  summarize(.groups='keep',
            n=n(),
            mean = mean(residual),
            l95 = lower(residual),
            u95 = upper(residual)) %>%
  ungroup() -> regional_smry

regional_smry %>%
  select(tx, region, n, mean, l95, u95) -> regional_smry_out
write_supp_table(regional_smry_out, 'Brain regional qPCR analysis of human siRNA sequences in vivo.')

par(mar=c(2,4,2,5))
xlims = range(region_meta$x) + c(-0.5, 0.5)
ylims = range(0, 1.5)
yats=0:6/4
ybigs=0:3/2
ybiglabs=percent(ybigs)
plot(NA, NA, xlim=xlims, ylim=ylims, axes=F, ann=F, xaxs='i', yaxs='i')
axis(side=1, at=xlims, labels=NA, lwd.ticks=0)
#par(xpd=T)
#text(x=region_meta$x, y=rep(-0.05, nrow(region_meta)), labels=region_meta$region, srt=45, adj=1)
#par(xpd=F)
mtext(side=1, line=0.25, at=region_meta$x, text=region_meta$region, cex=0.8)
axis(side=2, at=yats, tck = -0.02, labels=NA)
axis(side=2, at=ybigs, tck = -0.05, labels=NA)
axis(side=2, at=ybigs, lwd=0, labels=ybiglabs, las=2, line=-0.5)
mtext(side=2, line=2.75, text=expression(residual~italic(PRNP)), cex=0.8)
abline(h=1, lty=3)
barwidth=0.15
rect(xleft=regional_smry$x + regional_smry$x_offset - barwidth/2, xright=regional_smry$x + regional_smry$x_offset + barwidth/2, ybottom=rep(0, nrow(regional_smry)), ytop=regional_smry$mean, col=alpha(regional_smry$color, ci_alpha), border=NA)
arrows(x0=regional_smry$x + regional_smry$x_offset, y0=regional_smry$l95, y1=regional_smry$u95, code=3, angle=90, length=0.025, col=regional_smry$color)
points(x=regional_indivs$x + regional_indivs$x_offset, y=pmin(regional_indivs$residual, max(ylims)), pch=21, col=regional_indivs$color, bg='#FFFFFF', cex=0.5)
tx_compared_by_qpcr %>%
  arrange(x_offset) %>%
  distinct(tx,color) -> leg
par(xpd=T)
legend(x=max(xlims),y=max(ylims)*1.1,leg$tx,col=leg$color,pch=15,bty='n',cex=0.7)
par(xpd=F)
mtext(LETTERS[panel], side=3, cex=1, adj = -0.0, line = 0.5); panel = panel + 1

proc_smry %>%
  mutate(descno = substr(display_tx,1,4)) %>%
  mutate(scaffold = substr(display_tx,6,7)) %>%
  filter(descno %in% c('1035','2226','2439','2440','2520','2768')) %>%
  select(descno, scaffold, mean, color) %>%
  mutate(orig_scaffold = scaffold) %>%
  mutate(scaffold = case_when(scaffold %in% c('s1','s2') ~ 's1_2',
                              scaffold == 's4' ~ 's4')) %>%
  pivot_wider(names_from = scaffold, values_from=c(mean,color,orig_scaffold)) %>%
  select(-color_s1_2, orig_scaffold_s4) %>%
  rename(s1_2 = mean_s1_2, s4 = mean_s4) %>%
  mutate(diff = s4 - s1_2) %>%
  arrange(-s1_2) %>%
  mutate(x = row_number()) -> scafdiff

scafdiff %>%
  select(descno, s1_2, s4, diff) -> scafdiff_out
write_supp_table(scafdiff_out, 'Difference in target engagement between scaffolds for human siRNA sequences in vivo.')

par(mar=c(2,4,2,1))
xlims = range(scafdiff$x) + c(-0.5, 0.5)
ylims = c(0, 1)
yats=0:6/4
ybigs=0:3/2
ybiglabs=percent(ybigs)
plot(NA, NA, xlim=xlims, ylim=ylims, axes=F, ann=F, xaxs='i', yaxs='i')
axis(side=1, at=xlims, labels=NA, lwd.ticks=0)
mtext(side=1, line=0.25, at=scafdiff$x, text=scafdiff$descno, cex=0.6)
axis(side=2, at=yats, tck = -0.02, labels=NA)
axis(side=2, at=ybigs, tck = -0.05, labels=NA)
axis(side=2, at=ybigs, lwd=0, labels=ybiglabs, las=2, line=-0.5)
mtext(side=2, line=2.5, text='mean residual PrP', cex=0.8)
boxwidth = 0.5
rect(xleft=scafdiff$x - boxwidth/2, 
     xright=scafdiff$x + boxwidth/2, 
     ybottom = scafdiff$s4,
     ytop = scafdiff$s1_2, 
     col='#A9A9A9', border=NA)
segments(x0=scafdiff$x - boxwidth/2, 
         x1=scafdiff$x + boxwidth/2, 
         y0 = scafdiff$s4,lwd=2) 
text(x=scafdiff$x, y = scafdiff$s4, pos=1, labels='s4', cex=0.8) 
segments(x0=scafdiff$x - boxwidth/2, 
         x1=scafdiff$x + boxwidth/2, 
         y0 = scafdiff$s1_2,lwd=2)
text(x=scafdiff$x, y = scafdiff$s1_2, pos=3, labels=scafdiff$orig_scaffold_s1_2, cex=0.8) 
mtext(LETTERS[panel], side=3, cex=1, adj = -0.0, line = 0.5); panel = panel + 1

silence_is_golden = dev.off()
} #### end Figure 4 #### 



## Figure 5 beginning #### 
if ('figure-5'=='figure-5') {
  tell_user('done.\nCreating Figure 5...')
resx=300
png('display_items/figure-5.png',width=6.5*resx,height=4.0*resx,res=resx)
  
layout_matrix = matrix(c(1,1,1,1,1,12,
                         2,2,3,4,5,6,
                         2,2,3,7,8,9,
                         10,10,10,11,11,11), nrow=4, byrow=T)
layout(layout_matrix, heights = c(1.2,1,1,1.5), widths=c(1,1,.25,1,1,1))
#layout.show(9)

panel = 1

### diagram across top ####
par(mar=c(0.5,0,1.5,0))
raster_panel = image_convert(image_read('received/h2h_legend.png'),'png')
plot(as.raster(raster_panel))
mtext(side=3, adj=0.05, text=LETTERS[panel], line=0.0); panel = panel + 1

### H2H - ELISA #### 

scaffold_comparison = c('CMR-1871')

proc = process_elisas(scaffold_comparison, control_group = 'saline') %>%
  inner_join(meta, by='tx') %>%
  mutate(color = case_when(dose_dio==0 ~ color,
                           dose_dio==0.2 ~ color,
                           dose_dio==1 ~ color,
                           dose_dio==5 ~ color))
proc %>%
  distinct(tx, display_tx, is_reference, is_ntc, sequence_number, color, dose_dio) %>%
  mutate(is_exna = grepl('exNA',tx,ignore.case=T)) %>%
  mutate(is_hips = grepl('hiPS',tx,ignore.case=T)) %>%
  arrange(!is_reference, !is_ntc, !is_hips, desc(tx), is_exna, dose_dio) %>%
  mutate(x = row_number()) %>%
  mutate(y = max(x) - x + 1) %>%
  mutate(group_id = paste0(display_tx,' ',dose_dio,' nmol')) %>%
  mutate(short_disp = gsub('2439-','',gsub(' tail','',display_tx))) -> xes

proc %>%
  inner_join(xes %>% select(tx, short_disp, dose_dio, x, y), by=c('tx','dose_dio')) %>%
  mutate(pch=21) -> proc

proc %>%
  group_by(x, y, tx, display_tx, dose_dio, color) %>%
  summarize(.groups = 'keep',
            n = n(),
            mean = mean(rel),
            l95 = lower(rel),
            u95 = upper(rel),
            max = max(rel)) %>%
  ungroup() -> proc_smry

par(mar=c(2,5,2,1))
xlims = c(0,1.25)
ylims = range(proc$y) + c(-0.5, 0.5)
xats = 0:6/4
xbigs = 0:3/2
xbiglabs = percent(0:3/2)
plot(NA, NA, xlim=xlims, ylim=ylims, axes=F, ann=F, xaxs='i', yaxs='i')
abline(v=1, lty=3)
axis(side=1, at=xbigs, tck=-0.05, labels=NA)
axis(side=1, at=xats, tck=-0.02, labels=NA)
axis(side=1, at=xbigs, lwd=0, line=-0.75, labels=xbiglabs, cex=0.8)
mtext(side=3, line=0.25, text='residual PrP', cex=0.8)
axis(side=2, at=ylims, lwd.ticks=0, labels=NA)
mtext(side=2, at=proc_smry$y, line=0.25, text=nmol_to_ug(proc_smry$dose_dio), las=2, cex=0.6)
mtext(side=2, at=max(ylims) + 0.5, las=2, line=0.25, text='dose\n(µg)', cex=0.6, padj=0)
xes %>%
  group_by(display_tx, short_disp) %>%
  summarize(.groups='keep',
            miny = min(y),
            midy = mean(y),
            maxy = max(y)) %>%
  ungroup() -> tranches
tranche_line = 1.75
overhang_left = 0.4
overhang_right = 0.4
for (i in 1:nrow(tranches)) {
  axis(side=2, line=tranche_line, at=c(tranches$miny[i], tranches$maxy[i]) + c(-1,1)*c(overhang_left,overhang_right), tck=0.03, labels=NA)
  mtext(side=2, line=tranche_line+0.2, at=tranches$midy[i], adj=1, las=2, text=tranches$short_disp[i], cex=0.4)
}
barwidth=0.8
rect(xleft=rep(0, nrow(proc_smry)), xright=proc_smry$mean, ybottom=proc_smry$y-barwidth/2, , ytop=proc_smry$y+barwidth/2, col=alpha(proc_smry$color, ci_alpha), border=NA)
arrows(x0=proc_smry$l95, x1=proc_smry$u95, y0=proc_smry$y, col=proc_smry$color, lwd=1.5, code=3, angle=90, length=0.05)
points(x=proc$rel, y=proc$y, col=proc$color, bg='#FFFFFF', pch=21)
abline(v=min(proc_smry$mean), col=proc_smry %>% arrange(mean) %>% slice(1) %>% pull(color), lty=3, lwd=0.5)
mtext(side=3, adj=0.0, text=LETTERS[panel], line=0.5); panel = panel + 1

proc %>%
  select(study_id, plate, animal, genotype, sex, display_tx, dose_dio, dosing_regimen, days_harvest, ngml_av, saline_mean, rel) -> proc_out
write_supp_table(proc_out, 'Comparison of 2439 with different scaffolds and matched vs. fixed tail - individual animal data.')
proc_smry %>%
  select(display_tx, n, mean, l95, u95) -> proc_smry_out
write_supp_table(proc_smry_out, 'Comparison of 2439 with different scaffolds and matched vs. fixed tail - summarized.')




### H2H - qPCR regional #### 

# the qPCR spreadsheets use alterantive names for each scaffold. map
# these to the xes table from the H2H ELISA above.
name_map = read_tsv('data/analytic/qpcr_name_mapping.tsv',col_types=cols()) %>%
  inner_join(xes, by='group_id')

qpcr %>% 
  filter(study_id %in% scaffold_comparison) %>%
  filter(target=='PRNP') %>%
  group_by(study_id, qpcr_id, region, biorep, sample_group) %>%
  summarize(.groups='keep',
            n_techreps = n(),
            mean_twoddct = mean(twoddct)) %>%
  ungroup() %>%
  group_by(study_id, qpcr_id, region) %>%
  mutate(residual = mean_twoddct / mean(mean_twoddct[sample_group %in% c('saline','PBS')])) %>%
  ungroup() %>%
  inner_join(name_map, by=c('sample_group'='qpcr_name')) %>%
  filter(tx %in% c('fixed tail-exNA','matched tail-hiPS','fixed tail-hiPS')) -> h2h_qpcr

h2h_layout = read_tsv('data/miscellaneous/h2h_plate_layout.tsv', col_types=cols()) %>%
  mutate(well = paste0(row, col)) %>%
  mutate(id = as.character(id))


scaffold_levels_in_order = c('s2 matched', 's2 fixed', 's4 fixed')
regions_in_order = region_meta$region

h2h_qpcr %>%
  mutate(well = gsub('-.*','',gsub('CMR-1871-JES0034-','',biorep))) %>%
  inner_join(h2h_layout, by=c('well')) %>% 
  rename(scaffold = short_disp, animal=id) %>%
  select(scaffold, region, dose_dio, animal, residual, color) %>%
  mutate(scaffold = factor(scaffold, levels=scaffold_levels_in_order)) %>%
  mutate(region = factor(region, levels=regions_in_order)) -> h2h_model_data

h2h_model_data %>%
  select(animal, scaffold, region, dose_dio, residual) %>%
  arrange(scaffold, animal, region, dose_dio) -> h2h_model_out
write_supp_table(h2h_model_out, 'Regional qPCR data for scaffold/tail comparison.')

# overall model
lm_model = lm(residual ~ scaffold + log(dose_dio) + region, 
              data=h2h_model_data)
summary(lm_model)$coefficients %>%
  as_tibble(rownames='variable') %>%
  rename(estimate = Estimate, se = `Std. Error`, t=`t value`, p=`Pr(>|t|)`) -> h2h_coefs

write_supp_table(h2h_coefs, 'Linear model coefficients for regional qPCR data in scaffold/tail comparison.')

h2h_model_data %>%
  distinct(scaffold, color) -> h2h_leg

alpha_value = 0.05
ci_size = qnorm(1-alpha_value/2)
h2h_coefs %>%
  filter(grepl('Intercept|scaffold',variable)) %>%
  mutate(estimate = case_when(variable=='(Intercept)' ~ 0,
                              TRUE ~ estimate),
         variable = case_when(variable=='(Intercept)' ~ 's2 matched',
                              TRUE ~ gsub('scaffold','',variable))) %>%
  mutate(l95 = estimate - ci_size * se, u95 = estimate + ci_size * se) %>%
  mutate(x = row_number()) %>%
  inner_join(h2h_leg, by =c('variable'='scaffold')) -> scaffold_fits

h2h_coefs %>%
  filter(grepl('Intercept|region',variable)) %>%
  mutate(estimate = case_when(variable=='(Intercept)' ~ 0,
                              TRUE ~ estimate),
         variable = case_when(variable=='(Intercept)' ~ regions_in_order[1],
                              TRUE ~ gsub('region','',variable))) %>%
  mutate(l95 = estimate - ci_size * se, u95 = estimate + ci_size * se) %>%
  mutate(x = row_number()) -> region_fits


# nested anova version - Error(animal means treat animal as random effect)
# anova_model = aov(residual ~ scaffold + dose_dio + region + Error(animal), data=h2h_model_data)
# summary(anova_model)
# coefficients(anova_model)


### dose-response curves #### 

xlims = c(4,300)
xats = rep(1:9, 5) * rep(10^(-1:3),each=9)
xbigs = 10^(-1:3)
xlabs = nmol_to_ug(c(0.2, 1, 5))
xlablabs = xlabs
# xats = xats[xats >= min(xbigs) & xats <= max(xbigs)]
ylims = c(0, 1.5)
yats = 0:6/4
ybigs = 0:2/2
ybiglabs = percent(0:2/2)

# blanks for y axis flowover
par(mar=c(0,0,3,0))
plot(NA, NA, xlim=xlims, ylim=ylims, axes=F, ann=F, xaxs='i', yaxs='i', log='x')
##par(mar=c(3,0,0,0))
##plot(NA, NA, xlim=xlims, ylim=ylims, axes=F, ann=F, xaxs='i', yaxs='i', log='x')
mtext(side=3, adj=0.0, text=LETTERS[panel], line=1.0); panel = panel + 1

if(exists('ed50_table')) {
  rm(ed50_table)
}
ed50_table = tibble(region=character(0), scaffold=character(0), ed50=numeric(0))
for (i in 1:nrow(region_meta)) {
  if (i <= 3) {
    par(mar=c(0.125,0,2,0))
  } else {
    par(mar=c(2,0,0.125,0))
  }
  plot(NA, NA, xlim=xlims, ylim=ylims, axes=F, ann=F, xaxs='i', yaxs='i', log='x')
  axis(side=1, at=xats, tck=0.02, labels=NA)
  axis(side=1, at=xbigs, tck=0.05, labels=NA)

  axis(side=2, at=yats, tck=-0.02, labels=NA)
  axis(side=2, at=ybigs, tck=-0.05, labels=NA)
  if (i %in% c(1,4)) {
    axis(side=2, at=ybigs, labels=ybiglabs, line=-0.5, las=2, lwd=0)
  }
  abline(h=1, lty=3)
  if (i <= 3) {
  } else {
    axis(side=1, at=xlabs, lwd=0, line=-0.5, labels=xlablabs)
  }
  if (i==4) {
    mtext(side=2, at=max(ylims), text=expression(residual~italic(PRNP)), line=2.5, cex=0.8)
  }

  
  h2h_qpcr %>%
    filter(region==region_meta$region[i]) -> subs
  points(nmol_to_ug(subs$dose_dio), pmin(subs$residual, max(ylims)), pch=21, col=subs$color, bg='#FFFFFF')
  for (scaf in unique(subs$tx)) {
    subs %>% filter(tx==scaf) %>% mutate(ug = nmol_to_ug(dose_dio)) -> this_subs
    m = drm(residual ~ ug, data=this_subs, fct=LL.4(fixed=c(b=NA, c=0, d=1, e=NA)))
    x = seq(7,174,1)
    y = suppressWarnings(predict(m, newdata=data.frame(dose=x)))
    points(x, y, type='l', col=this_subs$color[1])
    ed50_obj = ED(m, 50, display=F)
    ed50 = as.numeric(ed50_obj[1,'Estimate'])
    this_row = tibble(region=region_meta$region[i], scaffold=this_subs$display_tx[1], ed50=ed50)
    # fix instances where the ed50 is a poor fit because even the top dose is >50% residual
    if (mean(this_subs$residual[this_subs$dose_dio==5]) > 0.5) {
      #this_row$ed50 = NA
    }
    ed50_table = rbind(ed50_table, this_row)
  }
  
  mtext(side=3, line=-0.75, text=region_meta$region[i], cex=0.8)
}


write_supp_table(ed50_table, 'ED50 values by brain region and scaffold for 2439 based on qPCR.')

### model fits #### 

par(mar=c(2,4,2,1))
xlims = range(scaffold_fits$x) + c(-0.5, 0.5)
ylims = c(-.35, .10)
ybigs = -8:8/20
ybiglabs = percent(ybigs, signed=T)
yats = seq(min(ylims),max(ylims),by=.01)
plot(NA, NA, xlim=xlims, ylim=ylims, axes=F, ann=F, xaxs='i', yaxs='i')
axis(side=1, at=xlims, labels=NA, lwd.ticks=0)
mtext(side=1, line=0.25, at=scaffold_fits$x, text=scaffold_fits$variable, cex=0.8)
axis(side=2, at=yats, tck = -0.02, labels=NA)
axis(side=2, at=ybigs, tck = -0.05, labels=NA)
axis(side=2, at=ybigs, lwd=0, labels=ybiglabs, las=2, line=-0.5)
abline(h=0, lty=3)
mtext(side=2, line=2.75, text='relative effect', cex=0.8)
barwidth = 0.8
segments(x0=scaffold_fits$x[1]-barwidth/2, x1=scaffold_fits$x[1]+barwidth/2, y0=0, col='#000000')
rect(xleft=scaffold_fits$x-barwidth/2, xright=scaffold_fits$x+barwidth/2,
     ybottom=scaffold_fits$estimate, ytop=rep(0,nrow(scaffold_fits)),
     col=alpha(scaffold_fits$color,ci_alpha), border=NA)
arrows(x0=scaffold_fits$x, y0=scaffold_fits$l95, y1=scaffold_fits$u95, code=3, angle=90, length=0.05)
mtext(side=3, text='effect of scaffold/tail', cex=0.8)
mtext(side=3, adj=0.0, text=LETTERS[panel], line=0.5); panel = panel + 1

xlims = range(region_fits$x) + c(-0.5, 0.5)
ylims = c(-.15, .25)
ybigs = -6:6/20
ybiglabs = percent(ybigs, signed=T)
yats = seq(min(ylims),max(ylims),by=.01)
plot(NA, NA, xlim=xlims, ylim=ylims, axes=F, ann=F, xaxs='i', yaxs='i')
axis(side=1, at=xlims, labels=NA, lwd.ticks=0)
mtext(side=1, line=0.25, at=region_fits$x, text=region_fits$variable, cex=0.8)
axis(side=2, at=yats, tck = -0.02, labels=NA)
axis(side=2, at=ybigs, tck = -0.05, labels=NA)
axis(side=2, at=ybigs, lwd=0, labels=ybiglabs, las=2, line=-0.5)
abline(h=0, lty=3)
mtext(side=2, line=2.75, text='relative effect', cex=0.8)
barwidth = 0.8
segments(x0=region_fits$x[1]-barwidth/2, x1=region_fits$x[1]+barwidth/2, y0=0, col='#000000')
rect(xleft=region_fits$x-barwidth/2, xright=region_fits$x+barwidth/2,
     ybottom=region_fits$estimate, ytop=rep(0,nrow(region_fits)),
     col=alpha('#A9A9A9',ci_alpha), border=NA)
arrows(x0=region_fits$x, y0=region_fits$l95, y1=region_fits$u95, code=3, angle=90, length=0.05)
mtext(side=3, text='effect of region', cex=0.8)
mtext(side=3, adj=0.0, text=LETTERS[panel], line=0.5); panel = panel + 1


### rest of legend at top ####
par(mar=c(1,0,2,0))
raster_panel = image_convert(image_read('received/oligo-legend.png'),'png')
plot(as.raster(raster_panel))



silence_is_golden = dev.off()
} ###  end of figure 5 #### 

### move to supp - Gfap, Iba1 #### 


qpcr %>%
  filter(study_id %in% scaffold_comparison) %>%
  filter(target %in% c('GFAP','IBA1')) %>%
  group_by(study_id, qpcr_id, target, region, biorep, sample_group) %>%
  summarize(.groups='keep',
            n_techreps = n(),
            mean_twoddct = mean(twoddct)) %>%
  ungroup() %>%
  group_by(study_id, qpcr_id, region, target) %>%
  mutate(residual = mean_twoddct / mean(mean_twoddct[sample_group %in% c('saline','PBS')])) %>%
  ungroup() -> inflam_qpcr



target_meta = tibble(target=c('GFAP','IBA1'),
                     target_disp = c('Gfap','Iba1'),
                     panel=c(1,2))

for (i in 1:nrow(target_meta)) {
  this_target = target_meta$target[i]
  
  
  inflam_qpcr %>%
    inner_join(name_map, by=c('sample_group'='qpcr_name')) %>%
    inner_join(target_meta, by='target') %>%
    filter(target==this_target) %>%
    select(group_id, tx, dose_dio, target, target_disp, region, residual, x, y, color, panel) %>%
    arrange(group_id) -> inflam_indivs
  
  control_group_values = inflam_indivs$residual[inflam_indivs$tx=='saline']
  
  inflam_indivs %>% 
    group_by(group_id, tx, dose_dio, target, target_disp, panel, x, y, color) %>%
    summarize(.groups='keep',
              n=n(),
              mean = mean(residual),
              l95 = lower(residual),
              u95 = upper(residual),
              ks_p = ks.test(residual,control_group_values)$p.value) %>%
    ungroup() %>%
    mutate(psymb = p_to_symbol(ks_p)) -> inflam_smry
  
 
  par(mar=c(2,0,2,0.5))
  xlims = range(0, 1.5) 
  ylims = range(xes$y) + c(-0.5, 0.5) 
  xats = 0:6/4 
  xbigs = 0:3/2 
  xbiglabs = percent(xbigs) 
  plot(NA, NA, xlim=xlims, ylim=ylims, axes=F, ann=F, xaxs='i', yaxs='i')
  axis(side=1, at=xbigs, tck=-0.05, labels=NA)
  axis(side=1, at=xats, tck=-0.02, labels=NA)
  axis(side=1, at=xbigs, lwd=0, line=-0.75, labels=xbiglabs, cex=0.8)
  mtext(side=3, line=0.25, text=target_meta$target_disp[i], cex=0.8, font=3)
  axis(side=2, at=ylims, lwd.ticks=0, labels=NA)
  abline(v=1, lty=3)
  barwidth = 0.5
  rect(ybottom=inflam_smry$y - barwidth/2, ytop=inflam_smry$y + barwidth/2, xleft=rep(0, nrow(inflam_smry)), xright=inflam_smry$mean, col=alpha(inflam_smry$color, ci_alpha), border=NA)
  arrows(y0=inflam_smry$y, x0=inflam_smry$l95, x1=inflam_smry$u95, code=3, angle=90, length=0.05, col=inflam_smry$color)
  points(x=inflam_indivs$residual, y=pmin(inflam_indivs$y, max(ylims)), pch=21, col=inflam_indivs$color, bg='#FFFFFF', cex=0.5)
  mtext(side=4, at=inflam_smry$y, text=inflam_smry$psymb, cex=0.5, las=2, line=0, adj=1)
  
}













## Figure S4 - genotype comparison #### 
tell_user('done.\nCreating Figure S4...')
if ('figure-s4'=='figure-s4') {

resx=300
png('display_items/figure-s4.png',width=6.5*resx,height=2.75*resx,res=resx)


layout_matrix = matrix(c(1,2,3,4), byrow=T, nrow=1)
layout(layout_matrix, widths=c(0.5, 1.0, 0.5, 1.0))
par(mar=c(3,4,4,1))
panel = 1

### prep #### 

gc_tg26 = process_elisas('CMR-1313', control_group='saline') %>%
  filter(grepl('2440|saline',tx)) %>%
  filter(dose_dio %in% c(0,10)) %>%
  inner_join(meta, by='tx') 
gc_tg25 = process_elisas('CMR-1418', control_group='saline') %>%
  filter(grepl('2440|saline',tx)) %>%
  filter(dose_dio %in% c(0,10)) %>%
  inner_join(meta, by='tx') 

gc_tg26 %>%
  distinct(tx, is_reference, is_ntc, sequence_number, color) %>%
  mutate(is_exna = grepl('exNA',tx,ignore.case=T)) %>%
  arrange(!is_reference, !is_ntc, sequence_number, is_exna) %>%
  mutate(x = row_number()) -> xes

gc_tg26$x = xes$x[match(gc_tg26$tx, xes$tx)]
gc_tg25$x = xes$x[match(gc_tg25$tx, xes$tx)]



### Tg26372 ELISA 2440 #### 

gc_tg26 %>%
  group_by(x, tx, color) %>%
  summarize(.groups='keep',
            n= n(),
            mean = mean(rel),
            l95 = lower(rel),
            u95 = upper(rel),
            max = max(rel)) %>%
  ungroup() -> gc_tg26_smry




xlims=range(gc_tg26$x)+c(-0.5,0.5)
ylims=c(0,1.5)
yats=0:6/4
ybigs=0:3/2
ybiglabs=percent(ybigs)
plot(NA, NA, xlim=xlims, ylim=ylims, axes=F, ann=F, xaxs='i', yaxs='i')
axis(side=1, at=xlims, labels=NA, lwd.ticks=0)
mtext(side=1, line=0.25, text='whole hemi', cex=0.8)
axis(side=2, at=yats, tck = -0.02, labels=NA)
axis(side=2, at=ybigs, tck = -0.05, labels=NA)
axis(side=2, at=ybigs, lwd=0, labels=ybiglabs, las=2, line=-0.5)
mtext(side=2, line=2.75, text='residual PrP', cex=0.8)
abline(h=1, lty=3)
barwidth=0.8
rect(xleft=gc_tg26_smry$x - barwidth/2, xright=gc_tg26_smry$x + barwidth/2, ybottom=rep(0, nrow(gc_tg26_smry)), ytop=gc_tg26_smry$mean, col=alpha(gc_tg26_smry$color, ci_alpha), border=NA)
arrows(x0=gc_tg26_smry$x, y0=gc_tg26_smry$l95, y1=gc_tg26_smry$u95, code=3, angle=90, length=0.05, col=gc_tg26_smry$color)
points(x=gc_tg26$x, y=pmin(gc_tg26$rel, max(ylims)), pch=21, col=gc_tg26$color, bg='#FFFFFF', cex=0.5)
par(xpd=T)
text(x=gc_tg26_smry$x, y=gc_tg26_smry$max+.15, labels=percent(gc_tg26_smry$mean,digits=1), srt=90, cex=0.7)
par(xpd=F)

gc_tg26_smry %>%
  distinct(tx, color) -> tx_leg
# par(xpd=T)
# legend(x=3.5, y=1.75, legend=tx_leg$tx, pch=15, col=tx_leg$color, bty='n', cex=0.8)
# par(xpd=F)

mtext(side=3, line=3, at=2.5, text='20x transgenics (Tg26372 hom)', cex=0.8, adj=0)
mtext(LETTERS[panel], side=3, cex=2, adj = -0.7, line = 0.5)
panel = panel + 1


#### Tg26372 regional of hiPS vs. exNA for 2440 #### 


regional_qpcr_studies = c('CMR-1313') # 2440 exNA & hiPS in 26372

qpcr %>%
  filter(study_id %in% regional_qpcr_studies) %>%
  filter(target=='PRNP') %>%
  group_by(study_id, qpcr_id, region, biorep, sample_group) %>%
  summarize(.groups='keep',
            n_techreps = n(),
            mean_twoddct = mean(twoddct)) %>%
  ungroup() %>%
  group_by(study_id, qpcr_id, region) %>%
  mutate(residual = mean_twoddct / mean(mean_twoddct[sample_group %in% c('saline','PBS')])) %>%
  ungroup() -> regional_qpcr


tx_compared_by_qpcr = tibble(tx_on_qpcr_plate=c('2440-hiPS','2440-exNA','saline','PBS'),
                             tx = c('2440-s1','2440-s4','saline','saline'),
                             x_offset = c(0, 0.25, -0.25, -0.25))
tx_compared_by_qpcr$color = meta$color[match(tx_compared_by_qpcr$tx, meta$display_tx)]

region_meta = tibble(region=c("HP", "PFC", "VC", "Str", "Thal", "CB"),
                     x = 1:6)

regional_qpcr %>%
  inner_join(tx_compared_by_qpcr, by=c('sample_group'='tx_on_qpcr_plate')) %>%
  select(tx, region, residual, x_offset, color) %>%
  filter(!is.na(residual)) %>%
  inner_join(region_meta, by='region') -> regional_indivs



xlims = range(region_meta$x) + c(-0.5, 0.5)
ylims = range(0, 1.5)

regional_indivs %>% 
  group_by(tx, region, x, x_offset, color) %>%
  summarize(.groups='keep',
            n=n(),
            mean = mean(residual),
            l95 = lower(residual),
            u95 = upper(residual),
            max = pmin(max(residual),max(ylims))) %>%
  ungroup() -> regional_smry

yats=0:6/4
ybigs=0:3/2
ybiglabs=percent(ybigs)
plot(NA, NA, xlim=xlims, ylim=ylims, axes=F, ann=F, xaxs='i', yaxs='i')
axis(side=1, at=xlims, labels=NA, lwd.ticks=0)
par(xpd=T)
text(x=region_meta$x, y=rep(-0.05, nrow(region_meta)), labels=region_meta$region, srt=45, adj=1)
par(xpd=F)
axis(side=2, at=yats, tck = -0.02, labels=NA)
axis(side=2, at=ybigs, tck = -0.05, labels=NA)
axis(side=2, at=ybigs, lwd=0, labels=ybiglabs, las=2, line=-0.5)
mtext(side=2, line=2.75, text=expression(residual~italic(PRNP)), cex=0.8)
abline(h=1, lty=3)
barwidth=0.25
rect(xleft=regional_smry$x + regional_smry$x_offset - barwidth/2, xright=regional_smry$x + regional_smry$x_offset + barwidth/2, ybottom=rep(0, nrow(regional_smry)), ytop=regional_smry$mean, col=alpha(regional_smry$color, ci_alpha), border=NA)
arrows(x0=regional_smry$x + regional_smry$x_offset, y0=regional_smry$l95, y1=regional_smry$u95, code=3, angle=90, length=0.025, col=regional_smry$color)
points(x=regional_indivs$x + regional_indivs$x_offset, y=pmin(regional_indivs$residual, max(ylims)), pch=21, col=regional_indivs$color, bg='#FFFFFF', cex=0.5)
par(xpd=T)
# text(x=regional_smry$x + regional_smry$x_offset, y=rep(max(ylims),nrow(regional_smry))+.1, labels=percent(regional_smry$mean,digits=1), srt=90, cex=0.6)
par(xpd=F)
mtext(LETTERS[panel], side=3, cex=2, adj = 0, line = 0.5)
panel = panel + 1



### Tg25109 ELISA 2440 #### 

gc_tg25 %>%
  group_by(x, tx, color) %>%
  summarize(.groups='keep',
            n= n(),
            mean = mean(rel),
            l95 = lower(rel),
            u95 = upper(rel),
            max = max(rel)) %>%
  ungroup() -> gc_tg25_smry

xlims=range(gc_tg25$x)+c(-0.5,0.5)
ylims=c(0,1.5)
yats=0:6/4
ybigs=0:3/2
ybiglabs=percent(ybigs)
plot(NA, NA, xlim=xlims, ylim=ylims, axes=F, ann=F, xaxs='i', yaxs='i')
axis(side=1, at=xlims, labels=NA, lwd.ticks=0)
mtext(side=1, line=0.25, text='whole hemi', cex=0.8)
axis(side=2, at=yats, tck = -0.02, labels=NA)
axis(side=2, at=ybigs, tck = -0.05, labels=NA)
axis(side=2, at=ybigs, lwd=0, labels=ybiglabs, las=2, line=-0.5)
mtext(side=2, line=2.75, text='residual PrP', cex=0.8)
abline(h=1, lty=3)
barwidth=0.8
rect(xleft=gc_tg25_smry$x - barwidth/2, xright=gc_tg25_smry$x + barwidth/2, ybottom=rep(0, nrow(gc_tg25_smry)), ytop=gc_tg25_smry$mean, col=alpha(gc_tg25_smry$color, ci_alpha), border=NA)
arrows(x0=gc_tg25_smry$x, y0=gc_tg25_smry$l95, y1=gc_tg25_smry$u95, code=3, angle=90, length=0.05, col=gc_tg25_smry$color)
points(x=gc_tg25$x, y=pmin(gc_tg25$rel, max(ylims)), pch=21, col=gc_tg25$color, bg='#FFFFFF', cex=0.5)
par(xpd=T)
text(x=gc_tg25_smry$x, y=gc_tg25_smry$max+.15, labels=percent(gc_tg25_smry$mean,digits=1), srt=90, cex=0.7)
par(xpd=F)
mtext(LETTERS[panel], side=3, cex=2, adj = -0.7, line = 0.5)
panel = panel + 1

gc_tg25_smry %>%
  distinct(tx, color) -> tx_leg
# par(xpd=T)
# legend(x=3.5, y=1.75, legend=tx_leg$tx, pch=15, col=tx_leg$color, bty='n', cex=0.8)
# par(xpd=F)

mtext(side=3, line=3, at=2.5, text='3x transgenics (Tg25109 het)', cex=0.8, adj=0)


#### Tg25109 regional of 2440 hiPS vs. exNA #### 


regional_qpcr_studies = c('CMR-1418') # 2440 exNA & hiPS in 25109

qpcr %>%
  filter(study_id %in% regional_qpcr_studies) %>%
  filter(target=='PRNP') %>%
  group_by(study_id, qpcr_id, region, biorep, sample_group) %>%
  summarize(.groups='keep',
            n_techreps = n(),
            mean_twoddct = mean(twoddct)) %>%
  ungroup() %>%
  group_by(study_id, qpcr_id, region) %>%
  mutate(residual = mean_twoddct / mean(mean_twoddct[sample_group %in% c('saline','PBS')])) %>%
  ungroup() -> regional_qpcr

tx_compared_by_qpcr = tibble(tx_on_qpcr_plate=c('2440-hiPS','2440-exNA','saline','PBS'),
                             tx = c('2440-s1','2440-s4','saline','saline'),
                             x_offset = c(0, 0.25, -0.25, -0.25))
tx_compared_by_qpcr$color = meta$color[match(tx_compared_by_qpcr$tx, meta$display_tx)]

region_meta = tibble(region=c("HP", "PFC", "VC", "Str", "Thal", "CB"),
                     x = 1:6)

regional_qpcr %>%
  inner_join(tx_compared_by_qpcr, by=c('sample_group'='tx_on_qpcr_plate')) %>%
  select(tx, region, residual, x_offset, color) %>%
  filter(!is.na(residual)) %>%
  inner_join(region_meta, by='region') -> regional_indivs

regional_indivs %>% 
  group_by(tx, region, x, x_offset, color) %>%
  summarize(.groups='keep',
            n=n(),
            mean = mean(residual),
            l95 = lower(residual),
            u95 = upper(residual),
            max = max(residual)) %>%
  ungroup() -> regional_smry


xlims = range(region_meta$x) + c(-0.5, 0.5)
ylims = range(0, 1.5)
yats=0:6/4
ybigs=0:3/2
ybiglabs=percent(ybigs)
plot(NA, NA, xlim=xlims, ylim=ylims, axes=F, ann=F, xaxs='i', yaxs='i')
axis(side=1, at=xlims, labels=NA, lwd.ticks=0)
par(xpd=T)
text(x=region_meta$x, y=rep(-0.05, nrow(region_meta)), labels=region_meta$region, srt=45, adj=1)
par(xpd=F)
axis(side=2, at=yats, tck = -0.02, labels=NA)
axis(side=2, at=ybigs, tck = -0.05, labels=NA)
axis(side=2, at=ybigs, lwd=0, labels=ybiglabs, las=2, line=-0.5)
mtext(side=2, line=2.75, text=expression(residual~italic(PRNP)), cex=0.8)
abline(h=1, lty=3)
barwidth=0.25
rect(xleft=regional_smry$x + regional_smry$x_offset - barwidth/2, xright=regional_smry$x + regional_smry$x_offset + barwidth/2, ybottom=rep(0, nrow(regional_smry)), ytop=regional_smry$mean, col=alpha(regional_smry$color, ci_alpha), border=NA)
arrows(x0=regional_smry$x + regional_smry$x_offset, y0=regional_smry$l95, y1=regional_smry$u95, code=3, angle=90, length=0.025, col=regional_smry$color)
points(x=regional_indivs$x + regional_indivs$x_offset, y=pmin(regional_indivs$residual, max(ylims)), pch=21, col=regional_indivs$color, bg='#FFFFFF', cex=0.5)
par(xpd=T)
# text(x=regional_smry$x + regional_smry$x_offset, y=regional_smry$max+.1, labels=percent(regional_smry$mean,digits=1), srt=90, cex=0.6)
par(xpd=F)

mtext(LETTERS[panel], side=3, cex=2, adj = 0, line = 0.5)
panel = panel + 1


tx_compared_by_qpcr %>%
  distinct(tx, color) -> tx_leg
par(xpd=T)
legend(x=3.5, y=1.75, legend=tx_leg$tx, pch=15, col=tx_leg$color, bty='n', cex=1)
par(xpd=F)

silence_is_golden = dev.off()
}



## Figure S5 - weights, Gfap, Iba1 #### 
tell_user('done.\nCreating Figure S5...')
if ('figure-s5'=='figure-s5') {
  resx=300
  png('display_items/figure-s5.png',width=6.5*resx,height=6*resx,res=resx)
  
  ### prep weights #### 
  weights_qpcr_studies = c('CMR-1313', 'CMR-1418', 'CMR-1465', 'CMR-1697', 'CMR-1871', 'CMR-2198', 'CMR-2371')
  
  process_elisas(weights_qpcr_studies, control_group='saline') %>%
    arrange(dose_dio, tx) %>%
    inner_join(meta, by='tx') -> proc
  
  proc %>%
    distinct(study_id, tx, is_reference, is_ntc, sequence_number, color, dose_dio) %>%
    mutate(is_exna = grepl('exNA',tx,ignore.case=T)) %>%
    arrange(study_id, !is_reference, !is_ntc, sequence_number, is_exna, tx, dose_dio) %>%
    mutate(x = row_number()) -> xes
  
  weight_deltas(weights_qpcr_studies) %>%
    inner_join(meta, by='tx') %>%
    inner_join(xes %>% select(study_id, tx, dose_dio, x), by=c('study_id','tx','dose_dio')) -> deltas
  
  deltas %>%
    select(study_id, animal, sex, display_tx, dose_dio, wt_date, weight, baseline, change) -> deltas_out
  write_supp_table(deltas_out, 'Animal weight change relative to baseline in target engagement studies.')
  
  deltas %>%
    mutate(wt_week = round(wt_date/7)) %>%
    group_by(study_id, tx, x, wt_week, color, display_tx, dose_dio) %>%
    summarize(.groups='keep',
              wt_dates = toString(unique(wt_date)),
              n = n(),
              mean=mean(change),
              l95 = lower(change),
              u95 = upper(change)) %>%
    ungroup() %>%
    mutate(disp = paste0(gsub(' tail','',display_tx), ' ', nmol_to_ug(dose_dio),' µg')) -> deltas_smry
  
  deltas_smry %>%
    filter(!is.na(x)) %>%
    arrange(x) -> deltas_smry
  
  deltas %>%
    distinct(tx) %>%
    arrange(desc(tx)) %>%
    pull() -> tx_in_order_with_saline_first
  deltas$tx_fct = factor(deltas$tx, levels=tx_in_order_with_saline_first)
  
  ### layout #### 
  
  layout_matrix = matrix(c(50,51,51,51,52,52,52,
                           1:49), nrow=8, byrow=T)
  layout(layout_matrix, widths=c(.35, rep(1,6)), heights=c(2, rep(1,6),.3))
  panel = 3
  
  
  par(mar=c(0.5,0.5,0.5,0))
  
  xlims = c(-3,35)
  ylims = c(-0.3, 0.3) 
  ybigs = -3:3/10
  xats = -1:34
  xbigs = 0:3*10
  
  # for (actual_panel in 1:4) {
  #   plot(NA, NA, xlim=xlims, ylim=ylims, axes=F, ann=F, xaxs='i', yaxs='i')
  #   axis(side=2, line=-2.5, at=ybigs, labels=percent(ybigs, signed=T), lwd=0, las=2)
  # }
  par(mar=c(0.5,0,0.5,0))
  
  this_x = 1
  for (i in 1:49) {
    if (i %% 7 == 1 & i != 43) {
      plot(NA, NA, xlim=xlims, ylim=ylims, axes=F, ann=F, xaxs='i', yaxs='i')
      axis(side=2, line=-3.2, at=ybigs, labels=percent(ybigs, signed=T), lwd=0, las=2, cex.axis=.7)
    } else if (i == 43) {
      plot(NA, NA, xlim=xlims, ylim=ylims, axes=F, ann=F, xaxs='i', yaxs='i')
    } else if (i >= 44) {
      plot(NA, NA, xlim=xlims, ylim=ylims, axes=F, ann=F, xaxs='i', yaxs='i')
      # axis(side=1, line=-3, at=xlims, lwd.ticks=0, labels=NA)
      axis(side=1, line=-2.5, at=xbigs, lwd=0, labels=xbigs)
    } else {
      plot(NA, NA, xlim=xlims, ylim=ylims, axes=F, ann=F, xaxs='i', yaxs='i')
      #axis(side=2, at=ybigs, labels=NA, tck=-0.05)
      #axis(side=2, at=ybigs, labels=percent(ybigs, signed=T), line=-0.5, lwd=0, las=2)
      abline(h=0, lty=2)
      subs = deltas %>% filter(x==this_x)
      subs_smry = deltas_smry %>% filter(x==this_x)
      for (anml in unique(deltas$animal)) {
        subsubs = subs %>% filter(animal==anml)
        points(subsubs$wt_date, subsubs$change, lwd=0.5, type='l', col=subsubs$color)
      }
      #points(subs_smry$wt_date, subs_smry$mean, lwd=1.5, type='l', col=subs_smry$color)
      #polygon(x=c(subs_smry$wt_date,rev(subs_smry$wt_date)), y=c(subs_smry$l95, rev(subs_smry$u95)), col=alpha(subs_smry$color[1], ci_alpha), border=NA)
      axis(side=2, line=0, at=ybigs, labels=NA, tck=-0.05)
      mtext(side=3, line=-0.5, at=0, adj=0, text=subs_smry$disp[1], col=subs_smry$color[1], cex=0.5)
      axis(side=1, line=0, at=xbigs, tck=-0.05, labels=NA)
      axis(side=1, line=0, at=xats, tck=-0.02, labels=NA)
      this_x = this_x + 1
    }
    
    if (i == 1) {
      mtext(side=3, adj=0.05, text=LETTERS[panel], line=0.0)
    }
  }
  
  xlims = 0:1
  ylims = c(0,1.5)
  ybigs = 0:6/4
  par(mar=c(3,0,2,0))
  plot(NA, NA, xlim=xlims, ylim=ylims, axes=F, ann=F, xaxs='i', yaxs='i')
  axis(side=2, line=-3.5, at=ybigs, labels=percent(ybigs), lwd=0, las=2)
  
  ### neuroinflam markers #### 
  
  inflam_qpcr_studies = c('CMR-1313', 'CMR-1418', 'CMR-1465', 'CMR-1697')
  qpcr %>%
    filter(study_id %in% inflam_qpcr_studies) %>%
    filter(target %in% c('GFAP','IBA1')) %>%
    inner_join(qpcr_txes, by=c('sample_group','study_id')) %>%
    group_by(study_id, qpcr_id, target, region, biorep, tx) %>%
    summarize(.groups='keep',
              n_techreps = n(),
              mean_twoddct = mean(twoddct)) %>%
    ungroup() %>%
    group_by(study_id, qpcr_id, region, target) %>%
    mutate(residual = mean_twoddct / mean(mean_twoddct[tx %in% c('saline')])) %>%
    ungroup() %>%
    filter(!is.na(tx)) %>%
    inner_join(meta, by='tx') %>%
    mutate(xid = paste0(study_id, tx)) %>%
    mutate(x = dense_rank(xid)) -> inflam_qpcr
  
  # statistics on GFAP & IBA1 - first set saline as refernece group
  
  inflam_qpcr %>%
    distinct(tx) %>%
    arrange(desc(tx)) %>% 
    pull() -> tx_in_order_with_saline_first
  
  inflam_qpcr$tx_fct = factor(inflam_qpcr$tx, levels=tx_in_order_with_saline_first)
  
  subs = inflam_qpcr %>% filter(target=='GFAP')
  DunnettTest(subs$residual, subs$tx_fct)
  
  subs = inflam_qpcr %>% filter(target=='IBA1')
  DunnettTest(subs$residual, subs$tx_fct)
  
  target_meta = tibble(target=c('GFAP','IBA1'),
                       target_disp = c('Gfap','Iba1'),
                       panel = c(1,2))
  
  inflam_qpcr %>% 
    inner_join(target_meta, by='target') %>%
    group_by(panel, target, target_disp, study_id, tx, display_tx, x, xid, color) %>%
    summarize(.groups='keep',
              n=n(),
              mean = mean(residual),
              l95 = lower(residual),
              u95 = upper(residual)) %>%
    ungroup() -> inflam_smry
  
  inflam_smry %>%
    group_by(target) %>%
    mutate(xrank = rank(mean, ties.method='first')) %>%
    mutate(xrank = max(xrank) - xrank + 1) %>% # reverse order on x axis
    ungroup() -> new_xes
  
  inflam_qpcr %>%
    inner_join(new_xes %>% select(target, xid, xrank), by=c('target','xid')) -> inflam_qpcr
  
  inflam_smry %>%
    inner_join(new_xes %>% select(target, xid, xrank), by=c('target','xid')) -> inflam_smry
  
  inflam_qpcr %>%
    select(study_id, display_tx, region, target, residual) -> inflam_qpcr_out
  write_supp_table(inflam_qpcr_out, 'Gfap and Iba1 inflammatory markers in target engagement studies - individual animal data.')

  inflam_smry %>%
    select(study_id, display_tx, target, n, mean, l95, u95) -> inflam_smry_out
  write_supp_table(inflam_smry_out, 'Gfap and Iba1 inflammatory markers in target engagement studies - summarized.')
  
  
  panel = 1
  for (this_panel in target_meta$panel) {
    indiv_subs = inflam_qpcr %>% filter(target == target_meta$target[target_meta$panel==this_panel])
    smry_subs = inflam_smry %>% filter(target == target_meta$target[target_meta$panel==this_panel])
    
    
    par(mar=c(3,0,2,1))
    xlims = range(new_xes$x) + c(-0.5, 0.5)
    ylims = range(0, 1.5)
    yats=0:6/4
    ybigs=0:3/2
    ybiglabs = percent(ybigs)
    ybiglabs[ybigs==1.5] = '≥150%'
    plot(NA, NA, xlim=xlims, ylim=ylims, axes=F, ann=F, xaxs='i', yaxs='i')
    axis(side=1, at=xlims, labels=NA, lwd.ticks=0)
    #axis(side=2, at=yats, tck = -0.02, labels=NA)
    #axis(side=2, at=ybigs, tck = -0.05, labels=NA)
    #axis(side=2, at=ybigs, lwd=0, labels=ybiglabs, las=2, line=-0.5)
    mtext(side=3, line=0, text=smry_subs$target_disp[1], cex=1, font=3)
    abline(h=1, lty=3)
    barwidth=0.8
    rect(xleft=smry_subs$xrank - barwidth/2, xright=smry_subs$xrank + barwidth/2, ybottom=rep(0, nrow(smry_subs)), ytop=smry_subs$mean, col=alpha(smry_subs$color, ci_alpha), border=NA)
    arrows(x0 =smry_subs$xrank   , y0=smry_subs$l95, y1=smry_subs$u95, code=3, angle=90, length=0.025, col=smry_subs$color)
    points(x  =indiv_subs$xrank , y=pmin(indiv_subs$residual, max(ylims)), pch=21, col=indiv_subs$color, bg='#FFFFFF', cex=0.5)
    # mtext(side=1, line=0.1, at=smry_subs$xrank, las=2, cex=0.8, text=paste0(smry_subs$study_id, '  ', smry_subs$display_tx))
    mtext(side=1, line=0.1, at=smry_subs$xrank, las=2, cex=0.6, text=smry_subs$display_tx)
    mtext(side=3, adj=0.05, text=LETTERS[panel], line=0.0); panel = panel + 1
  }
  
  
  
  
  silence_is_golden = dev.off()
}
### end Figure S5 ####


## Figure 6 - durability, dosing regimen, PD/PK, tox #### 
if ('figure-6'=='figure-6') {
  tell_user('done.\nCreating Figure 6...')
  resx=300
  png('display_items/figure-6.png',width=4.5*resx,height=5.0*resx,res=resx)
  
  layout_matrix = matrix(c(1,1,
                           2,3,
                           4,4), byrow=T, nrow=3)
  layout(layout_matrix, heights=c(.8,1,1), widths=c(1,1))
  
  panel = 1
  
  ### space for diagram across top ####
  par(mar=c(0.5,0,1.5,0))
  raster_panel = image_convert(image_read('received/oligo_scaffolds_2439seq.png'),'png')
  plot(as.raster(raster_panel))
  mtext(side=3, adj=0.05, text=LETTERS[panel], line=0.0); panel = panel + 1
  
  
  par(mar=c(3,4,3,1))

durability_study1 = c('CMR-2371') # 'CMR-2198',
process_elisas(durability_study1, control_group=c('none')) %>%
  arrange(dose_dio, days_harvest, tx) %>%
  filter(days_harvest %in% c(0,34,120)) %>%
  inner_join(meta, by='tx') %>%
  filter(display_tx %in% c('2439-s4','none','-')) %>%
  mutate(x = round(days_harvest / 30.44)) -> proc

xlims = c(0,5)
xbigs = c(1,4)
xbiglabs = c(34, 120)
xats = 0:5
ylims = c(0, 1.25)
ybigs = 0:3/2
yats = 0:6/4
plot(NA, NA, xlim=xlims, ylim=ylims, axes=F, ann=F, xaxs='i', yaxs='i')
axis(side=1, at=xbigs, tck=-0.05, labels=NA)
axis(side=1, at=xats, tck=-0.02, labels=NA)
axis(side=1, at=xbigs, lwd=0, line=-0.75, labels=xbiglabs, cex=0.8)
axis(side=2, at=yats, labels=NA, tck=-0.02)  
axis(side=2, at=ybigs, labels=NA, tck=-0.05)
axis(side=2, at=ybigs, labels=percent(ybigs), line=-0.75, lwd=0, las=2, cex=0.8)
mtext(side=2, line=2.25, text='residual PrP', cex=0.8)
abline(h=1, lty=3)
proc %>%
  group_by(x, display_tx, color) %>%
  summarize(.groups='keep',
            n = n(),
            mean = mean(rel),
            l95 = lower(rel),
            u95 = upper(rel),
            max = max(rel)) %>%
  ungroup() -> proc_smry
barwidth = 0.4
segments(x0=proc_smry$x-barwidth, x1=proc_smry$x+barwidth, y0=proc_smry$mean, col=proc_smry$color, lwd=1.5)
arrows(x0=proc_smry$x, y0=proc_smry$l95, y1=proc_smry$u95, code=3, angle=90, length=0.05, col=proc_smry$color, lwd=1.5)
points(x=proc$x, y=proc$rel, pch=20, col=alpha(proc$color,ci_alpha))
proc_smry %>%
  filter(display_tx != 'none') -> subs
par(xpd=T)
text(x=subs$x, y=subs$mean, pos=4, col=subs$color, labels=paste0('     ',percent(subs$mean, digits=1)), cex=0.6)
par(xpd=F)
mtext(side=3, line=0, text='single 348 µg dose', cex=0.8)
mtext(side=1, line=1, text='days post-dose', cex=0.8)
mtext(side=3, adj=-0.0, text=LETTERS[panel], line=0.5); panel = panel + 1


durability_study2 = c('CMR-2198') 
process_elisas(durability_study2, control_group=c('-')) %>%
  arrange(dose_dio, days_harvest, tx) %>%
  filter(days_harvest %in% c(0, 29, 91, 180)) %>%
  inner_join(meta, by='tx') %>%
  mutate(display_tx = gsub(' fixed tail','',display_tx)) %>%
  filter(grepl('s4|none', display_tx)) %>%
  mutate(x = round(days_harvest / 30.44)) -> proc

xlims = c(0,7)
xbigs = c(1, 3, 6)
xbiglabs = c(29, 91, 180)
xats = 0:7
ylims = c(0, 1.25)
ybigs = 0:3/2
yats = 0:6/4
plot(NA, NA, xlim=xlims, ylim=ylims, axes=F, ann=F, xaxs='i', yaxs='i')
axis(side=1, at=xbigs, tck=-0.05, labels=NA)
axis(side=1, at=xats, tck=-0.02, labels=NA)
axis(side=1, at=xbigs, lwd=0, line=-0.75, labels=xbiglabs, cex=0.8)
axis(side=2, at=yats, labels=NA, tck=-0.02)  
axis(side=2, at=ybigs, labels=NA, tck=-0.05)
axis(side=2, at=ybigs, labels=percent(ybigs), line=-0.75, lwd=0, las=2, cex=0.8)
mtext(side=2, line=2.25, text='residual PrP', cex=0.8)
abline(h=1, lty=3)
proc %>%
  group_by(x, display_tx, color) %>%
  summarize(.groups='keep',
            n = n(),
            mean = mean(rel),
            l95 = lower(rel),
            u95 = upper(rel),
            max = max(rel)) %>%
  ungroup() -> proc_smry
barwidth = 0.4
segments(x0=proc_smry$x-barwidth, x1=proc_smry$x+barwidth, y0=proc_smry$mean, col=proc_smry$color, lwd=1.5)
arrows(x0=proc_smry$x, y0=proc_smry$l95, y1=proc_smry$u95, code=3, angle=90, length=0.05, col=proc_smry$color, lwd=1.5)
points(x=proc$x, y=proc$rel, pch=20, col=alpha(proc$color,ci_alpha))
proc_smry %>%
  filter(display_tx != 'none') -> subs
par(xpd=T)
text(x=subs$x, y=subs$mean, pos=4, col=subs$color, labels=paste0('   ',percent(subs$mean, digits=1)), cex=0.6)
par(xpd=F)
mtext(side=3, line=0, text='single 174 µg dose', cex=0.8)
mtext(side=1, line=1, text='days post-dose', cex=0.8)
mtext(side=3, adj=-0.0, text=LETTERS[panel], line=0.5); panel = panel + 1

### CMR-3164 repeat dosing #### 

repeat_dose_study = c('CMR-3164')

proc = process_elisas(repeat_dose_study, control_group = 'none') %>%
  inner_join(meta, by='tx') %>%
  mutate(days_harvest = round(days_harvest/10)*10) %>%
  mutate(dosing_regimen = case_when(dosing_regimen == '0, 70, 90' ~ '0, 7, 90',
                                    TRUE ~ dosing_regimen)) 


proc %>%
  distinct(tx, dose_dio, dosing_regimen, days_harvest) %>%
  arrange(days_harvest, dose_dio) %>%
  mutate(x = row_number()) %>%
  mutate(y = max(x) - x + 1) %>%
  mutate(group_id = paste0(tx,' ',dose_dio,' ',dosing_regimen,' ',days_harvest)) -> xes

proc %>%
  inner_join(xes, by=c('tx', 'dose_dio', 'dosing_regimen','days_harvest')) %>%
  mutate(pch=21) -> proc

proc %>%
  group_by(x, y, tx, dose_dio,  dosing_regimen, days_harvest, color) %>%
  summarize(.groups = 'keep',
            n = n(),
            mean = mean(rel),
            l95 = lower(rel),
            u95 = upper(rel),
            max = max(rel)) %>%
  ungroup() %>%
  mutate(disp = paste0(nmol_to_ug(dose_dio), ' µg day ',dosing_regimen,', harvest day ',days_harvest)) -> proc_smry




par(mar=c(2,14,2,1))
xlims = c(0,1.60)
ylims = range(proc$y) + c(-0.5, 0.5)
xats = 0:6/4
xbigs = 0:3/2
xbiglabs = percent(0:3/2)
plot(NA, NA, xlim=xlims, ylim=ylims, axes=F, ann=F, xaxs='i', yaxs='i')
abline(v=1, lty=3)
axis(side=1, at=xbigs, tck=-0.05, labels=NA)
axis(side=1, at=xats, tck=-0.02, labels=NA)
axis(side=1, at=xbigs, lwd=0, line=-0.75, labels=xbiglabs, cex=0.8)
mtext(side=3, line=0.25, text='residual PrP', cex=0.8)
axis(side=2, at=ylims, lwd.ticks=0, labels=NA)
mtext(side=2, at=proc_smry$y, line=0.25, text=proc_smry$disp, las=2, cex=0.6)
mtext(side=2, at=max(ylims) + 0.5, las=2, line=0.25, text='dose\n(µg)', cex=0.6, padj=0)
barwidth=0.8
rect(xleft=rep(0, nrow(proc_smry)), xright=proc_smry$mean, ybottom=proc_smry$y-barwidth/2, , ytop=proc_smry$y+barwidth/2, col=alpha(proc_smry$color, ci_alpha), border=NA)
arrows(x0=proc_smry$l95, x1=proc_smry$u95, y0=proc_smry$y, col=proc_smry$color, lwd=1.5, code=3, angle=90, length=0.05)
points(x=proc$rel, y=proc$y, col=proc$color, bg='#FFFFFF', pch=21)
mtext(side=3, adj=-0.0, text=LETTERS[panel], line=0.5); panel = panel + 1



silence_is_golden = dev.off()
}


xes %>%
  distinct(tx, dose_dio, dosing_regimen) %>%
  mutate(offset = row_number()) %>%
  mutate(offset = (offset - mean(offset))/(n()+1)) -> offsets

undetermined_ct = 40
max_interpretable_ct = 35
qpcr %>%
  filter(study_id=='CMR-3164') %>%
  inner_join(study, by=c('sample_id'='animal','study_id')) %>%
  rename(animal = sample_id) %>%
  filter(!grepl('noCB',sample_group)) %>%
  mutate(region = gsub('.*_','',sample_group)) %>% 
  group_by(region) %>%
  mutate(target_ct = suppressWarnings(replace_na(as.numeric(target_ct),undetermined_ct)),
         control_ct = suppressWarnings(replace_na(as.numeric(control_ct),undetermined_ct))) %>%
  filter(target_ct <= max_interpretable_ct,
         control_ct <= max_interpretable_ct) %>%
  mutate(control_delta_ct = mean(target_ct[tx=='none'] - control_ct[tx=='none'])) %>%
  ungroup() %>%
  mutate(twoddct = 2^(control_delta_ct-(target_ct-control_ct))) %>%
  group_by(animal, region, tx, dose_dio, dosing_regimen) %>%
  summarize(.groups='keep', 
            delta_ct = mean(target_ct - control_ct),
            cv = sd(twoddct)/mean(twoddct)) %>%
  ungroup() %>%  
  filter(cv < 1.00) %>% # require %CV < 100%
  group_by(region) %>%
  mutate(control_delta_ct = mean(delta_ct[tx=='none'])) %>%
  mutate(twoddct = 2^(control_delta_ct - delta_ct)) %>% # now re-calculate twoddct AGAIN with the high-CV exclusions in place
  mutate(residual = twoddct / mean(twoddct[tx=='none'])) %>% # and normalize to mean of control group
  ungroup() %>%
  mutate(region = case_when(region=='Th' ~ 'Thal', TRUE ~ region)) %>%
  inner_join(region_meta, by='region') %>%
  rename(region_x = x) %>%
  inner_join(offsets, by=c('tx','dose_dio','dosing_regimen'), relationship='many-to-many') %>%
  mutate(x = region_x + offset) %>%
  inner_join(meta %>% select(tx, color, display_tx), by='tx') %>%
  mutate(y = max(region_x) - region_x - offset) -> repeatdose_qpcr 
  
repeatdose_qpcr %>%
  group_by(region, y, x, region_x, offset, display_tx, color, dose_dio, dosing_regimen) %>%
  summarize(.groups='keep',
            n = n(),
            mean = mean(residual),
            l95 = lower(residual),
            u95 = upper(residual)) %>%
  ungroup() %>%
  mutate(fulldisp = case_when(display_tx=='none' ~ 'none',
                              TRUE ~ paste(nmol_to_ug(dose_dio), 'µg day', dosing_regimen))) -> repeatdose_qpcr_smry


# 
# par(mar=c(2,4,2,1))
# xlims = range(repeatdose_qpcr_smry$x) + c(-0.5, 0.5)
# ylims = range(0, 2.5)
# yats=0:6/4
# ybigs=0:3/2
# ybiglabs=percent(ybigs)
# plot(NA, NA, xlim=xlims, ylim=ylims, axes=F, ann=F, xaxs='i', yaxs='i')
# axis(side=1, at=xlims, labels=NA, lwd.ticks=0)
# par(xpd=T)
# text(x=region_meta$x, y=rep(-0.05, nrow(region_meta)), labels=region_meta$region, srt=45, adj=1)
# par(xpd=F)
# axis(side=2, at=yats, tck = -0.02, labels=NA)
# axis(side=2, at=ybigs, tck = -0.05, labels=NA)
# axis(side=2, at=ybigs, lwd=0, labels=ybiglabs, las=2, line=-0.5)
# mtext(side=2, line=2.75, text='residual Prnp', cex=0.8)
# abline(h=1, lty=3)
# barwidth=1/6
# rect(xleft=repeatdose_qpcr_smry$x - barwidth/2, xright=repeatdose_qpcr_smry$x + barwidth/2, ybottom=rep(0, nrow(repeatdose_qpcr_smry)), ytop=repeatdose_qpcr_smry$mean, col=alpha(repeatdose_qpcr_smry$color, ci_alpha), border=NA)
# arrows(x0=repeatdose_qpcr_smry$x, y0=repeatdose_qpcr_smry$l95, y1=repeatdose_qpcr_smry$u95, code=3, angle=90, length=0.05, col=repeatdose_qpcr_smry$color)
# points(x=repeatdose_qpcr$x, y=pmin(repeatdose_qpcr$residual, max(ylims)), pch=21, col=repeatdose_qpcr$color, bg='#FFFFFF', cex=0.5)


## Figure S9 - regional repeat dose ####

## Figure S9 - other miscellanea ####
if ('figure-s9'=='figure-s9') {
  tell_user('done.\nCreating Figure S9...')
  png('display_items/figure-s9.png',width=3.25*resx,height=6.0*resx,res=resx)
  
  layout_matrix = matrix(c(1,1,
                           1,1,
                           2,2), nrow=3, byrow=T)
  layout(layout_matrix)
  panel =1 
  
  # transpose x & y
  par(mar=c(3,11,2,3))
  ylims = range(repeatdose_qpcr_smry$y) + c(-0.5, 0.5)
  xlims = range(0, 1.5)
  xats=0:6/4
  xbigs=0:3/2
  xbiglabs= c('0%','50%','100%','≥150%') # percent(xbigs)
  plot(NA, NA, xlim=xlims, ylim=ylims, axes=F, ann=F, xaxs='i', yaxs='i')
  axis(side=2, at=ylims, labels=NA, lwd.ticks=0)
  region_meta$y = max(region_meta$x) - region_meta$x
  mtext(side=2, at=region_meta$y, text=region_meta$region, las=2, line=8, cex=0.9)
  mtext(side=2, at=repeatdose_qpcr_smry$y, text=repeatdose_qpcr_smry$fulldisp, las=2, line=0.25, cex=0.6)
  axis(side=1, at=xats, tck = -0.02, labels=NA)
  axis(side=1, at=xbigs, tck = -0.05, labels=NA)
  axis(side=1, at=xbigs, lwd=0, labels=xbiglabs, line=-0.5)
  mtext(side=1, line=1.6, text=expression(residual~italic(PRNP)), cex=0.8)
  abline(v=1, lty=3)
  abline(v=.25, lty=3, lwd=0.375)
  barwidth=1/6
  rect(ybottom=repeatdose_qpcr_smry$y - barwidth/2, ytop=repeatdose_qpcr_smry$y + barwidth/2, xleft=rep(0, nrow(repeatdose_qpcr_smry)), xright=repeatdose_qpcr_smry$mean, col=alpha(repeatdose_qpcr_smry$color, ci_alpha), border=NA)
  arrows(y0=repeatdose_qpcr_smry$y, x0=repeatdose_qpcr_smry$l95, x1=repeatdose_qpcr_smry$u95, code=3, angle=90, length=0.03, col=repeatdose_qpcr_smry$color)
  points(y=repeatdose_qpcr$y, x=pmin(repeatdose_qpcr$residual, max(xlims)), pch=21, col=repeatdose_qpcr$color, bg='#FFFFFF', cex=0.5)
  mtext(side=3, adj=-0.0, text=LETTERS[panel], line=0.5); panel = panel + 1
  
  write_supp_table(repeatdose_qpcr_smry, 'Regional qPCR analysis for 2439-s4 repeat dose.')
  
  mouse_regional = c('CMR-1656')
  
  qpcr %>%
    filter(study_id %in% mouse_regional) %>%
    filter(target=='PRNP') %>%
    group_by(study_id, qpcr_id, region, biorep, sample_group) %>%
    summarize(.groups='keep',
              n_techreps = n(),
              mean_twoddct = mean(twoddct)) %>%
    ungroup() %>%
    group_by(study_id, qpcr_id, region) %>%
    mutate(residual = mean_twoddct / mean(mean_twoddct[sample_group %in% c('saline','PBS')])) %>%
    ungroup() -> regional_qpcr
  
  tx_compared_by_qpcr = tibble(tx_on_qpcr_plate=c('1682-exNA','saline'),
                               tx = c('1682-exNA','saline'),
                               display_tx = c('1682-s4','saline'),
                               x_offset = c(0.2, -0.2))
  tx_compared_by_qpcr$color = meta$color[match(tx_compared_by_qpcr$tx, meta$tx)]
  
  region_meta = tibble(region=c("HP", "PFC", "VC", "Str", "Thal", "CB"),
                       x = 1:6)
  
  regional_qpcr %>%
    inner_join(tx_compared_by_qpcr, by=c('sample_group'='tx_on_qpcr_plate')) %>%
    select(tx, region, residual, x_offset, color) %>%
    inner_join(region_meta, by='region') -> regional_indivs
  
  regional_indivs %>% 
    group_by(tx, region, x, x_offset, color) %>%
    summarize(.groups='keep',
              n=n(),
              mean = mean(residual),
              l95 = lower(residual),
              u95 = upper(residual)) %>%
    ungroup() -> regional_smry
  
  write_supp_table(regional_smry, 'Regional qPCR analysis for 1682-s4.')
  
  par(mar=c(2,4,2,3))
  xlims = range(region_meta$x) + c(-0.5, 0.5)
  ylims = range(0, 1.5)
  yats=0:6/4
  ybigs=0:3/2
  ybiglabs=percent(ybigs)
  plot(NA, NA, xlim=xlims, ylim=ylims, axes=F, ann=F, xaxs='i', yaxs='i')
  axis(side=1, at=xlims, labels=NA, lwd.ticks=0)
  par(xpd=T)
  text(x=region_meta$x, y=rep(-0.05, nrow(region_meta)), labels=region_meta$region, srt=45, adj=1)
  par(xpd=F)
  axis(side=2, at=yats, tck = -0.02, labels=NA)
  axis(side=2, at=ybigs, tck = -0.05, labels=NA)
  axis(side=2, at=ybigs, lwd=0, labels=ybiglabs, las=2, line=-0.5)
  mtext(side=2, line=2.75, text=expression(residual~italic(Prnp)), cex=0.8)
  abline(h=1, lty=3)
  abline(h=.25, lty=3, lwd=0.375)
  barwidth=0.4
  rect(xleft=regional_smry$x + regional_smry$x_offset - barwidth/2, xright=regional_smry$x + regional_smry$x_offset + barwidth/2, ybottom=rep(0, nrow(regional_smry)), ytop=regional_smry$mean, col=alpha(regional_smry$color, ci_alpha), border=NA)
  arrows(x0=regional_smry$x + regional_smry$x_offset, y0=regional_smry$l95, y1=regional_smry$u95, code=3, angle=90, length=0.05, col=regional_smry$color)
  points(x=regional_indivs$x + regional_indivs$x_offset, y=pmin(regional_indivs$residual, max(ylims)), pch=21, col=regional_indivs$color, bg='#FFFFFF', cex=0.5)
  
  par(xpd=T)
  legend(x=6, y=1.5, tx_compared_by_qpcr$display_tx, col=alpha(tx_compared_by_qpcr$color,ci_alpha), pch=15, bty='n', cex=0.8)
  par(xpd=F)
  mtext(side=3, adj=-0.0, text=LETTERS[panel], line=0.5); panel = panel + 1
  silence_is_golden = dev.off()
}
### end Figure S9 ####



## Figure S6 - ELISA QC #### 
resx=300
png('display_items/figure-s6.png',width=5*resx,height=5*resx,res=resx)
tell_user('done.\nCreating Figure S6...')
elisa_llq = 0.05
qc_meta = tibble(qc=c('MoNeg','MoPosLo','MoPosMid','MoPosHi'),
                 color = c('#AA2222','#AA22AA','#2222AA','#222222'),
                 qcrank = 4:1,
                 expected = c(0, .1, .5, 1))
llq_col = '#BBBBBB'
elisa_qc = read_tsv('data/analytic/elisa_qc.tsv', col_types=cols()) %>%
  mutate(platerank = dense_rank(plate)) %>%
  inner_join(qc_meta, by='qc')
elisa_qc %>%
  distinct(platerank, plate, study_id) %>%
  mutate(disp = paste0('plate ',plate,' study ',study_id)) -> elisa_qc_xes
xlims = range(elisa_qc$platerank) + c(-1,1)
ylims = c(0, 1.25)
par(mar=c(7,4,1,8))
plot(NA, NA, xlim=xlims, ylim=ylims, axes=F, ann=F, xaxs='i', yaxs='i')
axis(side=1, at=range(elisa_qc$platerank), lwd.ticks=0, labels=NA)
mtext(side=1, at=elisa_qc_xes$platerank, text=elisa_qc_xes$disp, las=2, cex=0.6)
axis(side=2, at=0:6/4, labels=NA)
axis(side=2, at=0:6/4, labels=percent(0:6/4), line=-0.5, las=2, lwd=0)
mtext(side=2, line=2.5, text='PrP (% of high QC)')
elisa_qc %>%
  filter(qc=='MoPosHi') %>%
  distinct(platerank, plate, llqrel) -> llqs
#points(x=llqs$platerank, y=llqs$llqrel, type='l', lty=3, col=qc_meta$color[qc_meta$qc=='MoNeg'])
polygon(x=c(llqs$platerank, rev(llqs$platerank)), y=c(llqs$llqrel,rep(0,nrow(llqs))), col=llq_col, border=NA)
for (this_qc in qc_meta$qc) {
  subs = elisa_qc %>% filter(qc %in% this_qc)
  points(subs$platerank, subs$qcrel, pch=20, cex=0.5, col=subs$color)
  subs %>% 
    group_by(platerank, plate, color) %>% 
    summarize(.groups='keep', 
              mean=mean(qcrel, na.rm=T)) %>% 
    ungroup() -> subsmry
  points(subsmry$platerank, subsmry$mean, type='l', lwd=1, col=subsmry$color)
}
abline(h=.5, lty=3, col=qc_meta$color[qc_meta$qc=='MoPosMid'])
abline(h=.1, lty=3, col=qc_meta$color[qc_meta$qc=='MoPosLo'])
mtext(side=4, at=0.5, las=2, col=qc_meta$color[qc_meta$qc=='MoPosMid'], text='expected mid QC', cex=0.7)
mtext(side=4, at=0.1, las=2, col=qc_meta$color[qc_meta$qc=='MoPosLo'], text='expected low QC', cex=0.7)
mtext(side=4, at=0.06, las=2, col=qc_meta$color[qc_meta$qc=='MoNeg'], text='negative QC', cex=0.7)
mtext(side=4, at=0.02, las=2, col=llq_col, text='plate-specific LLQ', cex=0.7)



silence_is_golden = dev.off()


elisa_qc %>%
  filter(!is.na(qcrel)) %>%
  group_by(platerank, plate, study_id, qc) %>%
  summarize(.groups='keep',
            mean_rel=mean(qcrel)) %>%
  ungroup() -> elisa_qc_platemeans

elisa_qc_platemeans %>%
  filter(!is.na(mean_rel)) %>%
  inner_join(qc_meta, by='qc') %>%
  group_by(qcrank, qc, expected) %>%
  summarize(.groups='keep',
            n=n(),
            min=min(mean_rel),
            mean=mean(mean_rel),
            max=max(mean_rel),
            median = median(mean_rel),
            q25 = quantile(mean_rel, .25),
            q75 = quantile(mean_rel, .75)) %>%
  ungroup() %>%
  arrange(qcrank) %>%
  select(-qcrank) -> elisa_qc_summary_stats

write_supp_table(elisa_qc_platemeans %>%
                   rename(relative_to_hi_qc = mean_rel), 'ELISA QC performance by plate.')
write_supp_table(elisa_qc_summary_stats, 'ELISA QC performance summarized across all plates.')



## Figure S8 - durability for 2439-s2 & s5 ####
if ('figure-s8'=='figure-s8') {
  tell_user('done.\nCreating Figure S8...')
  png('display_items/figure-s8.png',width=6.5*resx,height=2.5*resx,res=resx)
  
  layout_matrix = matrix(1:2, nrow=1, byrow=T)
  layout(layout_matrix)
  panel = 1
  
  par(mar=c(3,4,3,1))
  
  durability_study1 = c('CMR-2371') # 'CMR-2198',
  process_elisas(durability_study1, control_group=c('none')) %>%
    arrange(dose_dio, days_harvest, tx) %>%
    filter(days_harvest %in% c(0,34,120)) %>%
    inner_join(meta, by='tx') %>% 
    filter(display_tx %in% c('2439-s5','none','-')) %>%
    mutate(x = round(days_harvest / 30.44)) -> proc
  
  xlims = c(0,5)
  xbiglabs = xbigs = c(1,4)
  xats = 0:5
  ylims = c(0, 1.25)
  ybigs = 0:3/2
  yats = 0:6/4
  plot(NA, NA, xlim=xlims, ylim=ylims, axes=F, ann=F, xaxs='i', yaxs='i')
  axis(side=1, at=xbigs, tck=-0.05, labels=NA)
  axis(side=1, at=xats, tck=-0.02, labels=NA)
  axis(side=1, at=xbigs, lwd=0, line=-0.75, labels=xbiglabs, cex=0.8)
  axis(side=2, at=yats, labels=NA, tck=-0.02)  
  axis(side=2, at=ybigs, labels=NA, tck=-0.05)
  axis(side=2, at=ybigs, labels=percent(ybigs), line=-0.75, lwd=0, las=2, cex=0.8)
  mtext(side=2, line=2.25, text='residual PrP', cex=0.8)
  abline(h=1, lty=3)
  proc %>%
    group_by(x, display_tx, color) %>%
    summarize(.groups='keep',
              n = n(),
              mean = mean(rel),
              l95 = lower(rel),
              u95 = upper(rel),
              max = max(rel)) %>%
    ungroup() -> proc_smry
  barwidth = 0.4
  segments(x0=proc_smry$x-barwidth, x1=proc_smry$x+barwidth, y0=proc_smry$mean, col=proc_smry$color, lwd=1.5)
  arrows(x0=proc_smry$x, y0=proc_smry$l95, y1=proc_smry$u95, code=3, angle=90, length=0.05, col=proc_smry$color, lwd=1.5)
  points(x=proc$x, y=proc$rel, pch=20, col=alpha(proc$color,ci_alpha))
  proc_smry %>%
    filter(display_tx != 'none') -> subs
  par(xpd=T)
  text(x=subs$x, y=subs$mean, pos=4, col=subs$color, labels=paste0('     ',percent(subs$mean, digits=1)), cex=0.6)
  par(xpd=F)
  mtext(side=3, line=1, text=subs$display_tx[1], cex=0.8, col=subs$color[1])
  mtext(side=3, line=0, text='single 348 µg dose', cex=0.8)
  mtext(side=1, line=1, text='months post-dose', cex=0.8)
  mtext(side=3, adj=-0.0, text=LETTERS[panel], line=0.5); panel = panel + 1
  
  
  durability_study2 = c('CMR-2198') 
  process_elisas(durability_study2, control_group=c('-')) %>%
    arrange(dose_dio, days_harvest, tx) %>%
    filter(days_harvest %in% c(0, 29, 91, 180)) %>%
    inner_join(meta, by='tx') %>%
    mutate(display_tx = gsub(' fixed tail','',display_tx)) %>%
    filter(grepl('s2|none', display_tx)) %>%
    mutate(x = round(days_harvest / 30.44)) -> proc
  
  xlims = c(0,7)
  xbiglabs = xbigs = c(1, 3, 6)
  xats = 0:7
  ylims = c(0, 1.25)
  ybigs = 0:3/2
  yats = 0:6/4
  plot(NA, NA, xlim=xlims, ylim=ylims, axes=F, ann=F, xaxs='i', yaxs='i')
  axis(side=1, at=xbigs, tck=-0.05, labels=NA)
  axis(side=1, at=xats, tck=-0.02, labels=NA)
  axis(side=1, at=xbigs, lwd=0, line=-0.75, labels=xbiglabs, cex=0.8)
  axis(side=2, at=yats, labels=NA, tck=-0.02)  
  axis(side=2, at=ybigs, labels=NA, tck=-0.05)
  axis(side=2, at=ybigs, labels=percent(ybigs), line=-0.75, lwd=0, las=2, cex=0.8)
  mtext(side=2, line=2.25, text='residual PrP', cex=0.8)
  abline(h=1, lty=3)
  proc %>%
    group_by(x, display_tx, color) %>%
    summarize(.groups='keep',
              n = n(),
              mean = mean(rel),
              l95 = lower(rel),
              u95 = upper(rel),
              max = max(rel)) %>%
    ungroup() -> proc_smry
  barwidth = 0.4
  segments(x0=proc_smry$x-barwidth, x1=proc_smry$x+barwidth, y0=proc_smry$mean, col=proc_smry$color, lwd=1.5)
  arrows(x0=proc_smry$x, y0=proc_smry$l95, y1=proc_smry$u95, code=3, angle=90, length=0.05, col=proc_smry$color, lwd=1.5)
  points(x=proc$x, y=proc$rel, pch=20, col=alpha(proc$color,ci_alpha))
  proc_smry %>%
    filter(display_tx != 'none') -> subs
  par(xpd=T)
  text(x=subs$x, y=subs$mean, pos=4, col=subs$color, labels=paste0('   ',percent(subs$mean, digits=1)), cex=0.6)
  par(xpd=F)
  mtext(side=3, line=1, text=subs$display_tx[1], cex=0.8, col=subs$color[1])
  mtext(side=3, line=0, text='single 174 µg dose', cex=0.8)
  mtext(side=1, line=1, text='months post-dose', cex=0.8)
  mtext(side=3, adj=-0.0, text=LETTERS[panel], line=0.5); panel = panel + 1
  
  silence_is_golden = dev.off()
}
### end Figure S8 ####







## Off-targets #### 

tell_user('done.\nPerforming off-target analysis...')

blast_analyses = tribble(
  ~species, ~file,
  'human', 'data/off_targets/E90AWU40016-homo-sapiens.csv'
)

for (i in 1:nrow(blast_analyses)) {
  blast = read_csv(blast_analyses$file[i], col_types=cols()) %>% 
    clean_names() %>%
    mutate(gene_symbol = gsub('\\(|\\)|,','',str_extract(hit_def,'\\([A-Za-z0-9-]+\\),')))
  antisense_frame = 1 # you can check this for human - blast %>% filter(gene_symbol=='PRNP') %>% pull(hsp_hit_frame)
  max_length = max(nchar(blast$hsp_qseq))
  blast %>%
    filter(hsp_hit_frame == antisense_frame) %>%
    arrange(desc(hsp_identity)) %>%
    group_by(gene_symbol) %>%
    slice(1) %>%
    ungroup() %>%
    mutate(mismatches = max_length - hsp_identity) %>%
    select(gene_symbol, hit_accession, mismatches, hsp_qseq, hsp_hseq, hsp_midline) %>%
    arrange(mismatches, gene_symbol) -> blast_hits
  
  write_supp_table(blast_hits, paste0('Off-target hits for ',blast_analyses$species[i],'for 2439-exNA antisense seed region 2-17.'))
  
}



# SUPPLEMENT #### 

tell_user('done.\nFinalizing supplementary tables...')

# write the supplement directory / table of contents
supplement_directory %>% rename(table_number = name, description=title) -> contents
addWorksheet(supplement,'contents')
bold_style = createStyle(textDecoration = "Bold")
writeData(supplement,'contents',contents,headerStyle=bold_style,withFilter=T)
freezePane(supplement,'contents',firstRow=T)
# move directory to the front
original_order = worksheetOrder(supplement)
n_sheets = length(original_order)
new_order = c(n_sheets, 1:(n_sheets-1))
worksheetOrder(supplement) = new_order
activeSheet(supplement) = 'contents'
# now save
saveWorkbook(supplement,supplement_path,overwrite = TRUE)

elapsed_time = Sys.time() - overall_start_time
cat(file=stderr(), paste0('done.\nAll tasks complete in ',round(as.numeric(elapsed_time),1),' ',units(elapsed_time),'.\n'))

  