import xml.etree.ElementTree as ET
import csv
import argparse

# Set up command-line argument parsing
parser = argparse.ArgumentParser(description='Convert XML to CSV.')
parser.add_argument('xml_file', help='Path to the XML file')
parser.add_argument('csv_file', help='Path to the output CSV file')
args = parser.parse_args()

# Parse XML file
tree = ET.parse(args.xml_file)
root = tree.getroot()

# Define CSV header
csv_header = [
    "Hit_num", "Hit_id", "Hit_def", "Hit_accession", "Hit_len",
    "Hsp_num", "Hsp_bit-score", "Hsp_score", "Hsp_evalue",
    "Hsp_query-from", "Hsp_query-to", "Hsp_hit-from", "Hsp_hit-to",
    "Hsp_query-frame", "Hsp_hit-frame", "Hsp_identity", "Hsp_positive",
    "Hsp_gaps", "Hsp_align-len", "Hsp_qseq", "Hsp_hseq", "Hsp_midline"
]

# List to hold CSV rows
csv_rows = []

# Iterate over each Hit element
for hit in root.findall('Hit'):
    hit_num = hit.find('Hit_num').text
    hit_id = hit.find('Hit_id').text
    hit_def = hit.find('Hit_def').text
    hit_accession = hit.find('Hit_accession').text
    hit_len = hit.find('Hit_len').text
    
    for hsp in hit.find('Hit_hsps').findall('Hsp'):
        hsp_num = hsp.find('Hsp_num').text
        hsp_bit_score = hsp.find('Hsp_bit-score').text
        hsp_score = hsp.find('Hsp_score').text
        hsp_evalue = hsp.find('Hsp_evalue').text
        hsp_query_from = hsp.find('Hsp_query-from').text
        hsp_query_to = hsp.find('Hsp_query-to').text
        hsp_hit_from = hsp.find('Hsp_hit-from').text
        hsp_hit_to = hsp.find('Hsp_hit-to').text
        hsp_query_frame = hsp.find('Hsp_query-frame').text
        hsp_hit_frame = hsp.find('Hsp_hit-frame').text
        hsp_identity = hsp.find('Hsp_identity').text
        hsp_positive = hsp.find('Hsp_positive').text
        hsp_gaps = hsp.find('Hsp_gaps').text
        hsp_align_len = hsp.find('Hsp_align-len').text
        hsp_qseq = hsp.find('Hsp_qseq').text
        hsp_hseq = hsp.find('Hsp_hseq').text
        hsp_midline = hsp.find('Hsp_midline').text
        
        # Append row to list
        csv_rows.append([
            hit_num, hit_id, hit_def, hit_accession, hit_len,
            hsp_num, hsp_bit_score, hsp_score, hsp_evalue,
            hsp_query_from, hsp_query_to, hsp_hit_from, hsp_hit_to,
            hsp_query_frame, hsp_hit_frame, hsp_identity, hsp_positive,
            hsp_gaps, hsp_align_len, hsp_qseq, hsp_hseq, hsp_midline
        ])

# Write to CSV file
with open(args.csv_file, 'w', newline='') as file:
    writer = csv.writer(file)
    writer.writerow(csv_header)
    writer.writerows(csv_rows)

print(f'CSV file has been written to {args.csv_file}')
