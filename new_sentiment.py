import os
import glob
import ast
import json
import urllib
import csv

positive = {}

cwd = os.getcwd()
print cwd
path = '/speech/*.txt'
loc = cwd + path
files = glob.glob(loc)

with open('sentiment.csv', 'w') as csvfile:
    fieldnames = ['Prez', 'Date', 'Positive', 'Neutral', 'Negative']
    writer = csv.DictWriter(csvfile, fieldnames=fieldnames)
    writer.writeheader()
    
    for name in files:
		with open(name) as f:
			key = name.split("/")[9]
			key = key.strip()
			key = key.split(".txt")[0]
			prez = key.split("-")[0]
			date = key.split("-")[1]
			print key
			text = f.read()
			if len(text)<50000:
				data = urllib.urlencode({"text": text.encode('utf-8')})
				u = urllib.urlopen("http://text-processing.com/api/sentiment/", data)
				the_page = u.read()
				prob = ast.literal_eval(the_page)['probability']
				pos = prob['pos'] if prob['pos'] else 0
				positive[key] = pos
				neutral = prob['neutral']
				neg = prob['neg'] if prob['neg'] else 0
				print pos, neutral, neg
				writer.writerow({'Prez': prez, 'Date':date, 'Positive' : pos, 'Neutral' : neutral, 'Negative' : neg })
	
