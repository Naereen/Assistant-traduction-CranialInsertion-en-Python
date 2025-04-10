#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
This script downloads all the articles, that are available in both English and French, from the cranial-insertion.com website and saves them in the "all-articles" directory.

Each article gets downloaded as all-articles/article_id.en.txt and all-articles/article_id.fr.txt.
"""

import re
import subprocess

from tqdm import tqdm
import requests
from bs4 import BeautifulSoup
import os


# The script assumes that you have already logged in to the website and saved the cookies in a file named "cranial-insertion.com.cookies".
with open("cranial-insertion.com.cookies", "r") as cookie_file:
    loggedin = cookie_file.readline()

articles_list_url = "https://www.cranial-insertion.com/staff/articles"

response = requests.get(articles_list_url, cookies={"loggedin": loggedin, "siteLang": 'fr'})

soup = BeautifulSoup(response.text, 'html.parser')

# Find the table with the article list and extract the article IDs and their corresponding languages (English or French)
articles_id_english = []
articles_id_french = []

french_trs = []

articlelisttable = soup.find(id="articlelisttable")
for tr in tqdm(list(articlelisttable.find_all("tr"))[13:]):
    if 'Français' in str(tr):
        french_trs.append(tr)
        all_found = re.findall(r'([0-9]+):([0-9]+)', str(tr))
        if all_found:
            id_en, id_fr = all_found[0]
            articles_id_english.append(int(id_en))
            articles_id_french.append(int(id_fr))
            print(f"We found a new article of English ID = {id_en}, and French ID = {id_fr}...")

# Now we have the list of articles IDs and their corresponding languages (English or French) Let's download each article
for i in tqdm(range(len(articles_id_english))):
    id_en = articles_id_english[i]
    id_fr = articles_id_french[i]
    print(f"\n==> Let's download the pair of articles, of English ID = {id_en}, and French ID = {id_fr}...")

    file_en_path = f"all-articles/id-en-{id_en}_id-fr-{id_fr}.en.txt"
    file_fr_path = f"all-articles/id-en-{id_en}_id-fr-{id_fr}.fr.txt"

    # Check if the English article file already exists
    if os.path.exists(file_en_path):
        print(f"File {file_en_path} already exists. Skipping download...")
    else:
        # Download the English article
        url_en = f"https://www.cranial-insertion.com/staff/articles/{id_en}/edit"
        response_en = requests.get(url_en, cookies={"loggedin": loggedin, "siteLang": 'fr'})
        soup_en = BeautifulSoup(response_en.text, 'html.parser')
        thisArticleText_en = soup_en.find(id="thisArticleText").get_text()

        with open(file_en_path, "w") as file_en:
            file_en.write(thisArticleText_en)


    # Check if the French article file already exists
    if os.path.exists(file_fr_path):
        print(f"File {file_fr_path} already exists. Skipping download...")
    else:
        # Download the French article
        url_fr = f"https://www.cranial-insertion.com/staff/articles/{id_fr}/edit"
        response_fr = requests.get(url_fr, cookies={"loggedin": loggedin, "siteLang": 'fr'})
        soup_fr = BeautifulSoup(response_fr.text, 'html.parser')
        thisArticleText_fr = soup_fr.find(id="thisArticleText").get_text()

        with open(file_fr_path, "w") as file_fr:
            file_fr.write(thisArticleText_fr)

    print(f"We've downloaded fresh copies of the raw text from these two articles (or they were already there):")
    for file_path in [file_en_path, file_fr_path]:
        result = subprocess.run(["ls", "-larth", file_path], capture_output=True, text=True)
        print(result.stdout)
