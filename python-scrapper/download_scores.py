import urllib2
from BeautifulSoup import BeautifulSoup
from win32com.client import Dispatch
import csv, sys, os
import codecs
import time
import datetime
import win32file
import random
import re

def get_data(majors):
    for con in majors:
        print con['href']
        url=con['href']
        url = 'https://servicios.dgae.unam.mx/Febrero2009/resultados/' + url
        request = urllib2.Request( url )
        page = urllib2.urlopen( request )
        soup = BeautifulSoup(page)
        #write a copy of the contents to a local file
        filename = re.search('[0-9]+\.html',url)
        f = open(filename.group(0), 'w')
        f.write(str(soup))
        f.close
        data = []
        for i in soup.body:
          if i.string is not None:
            data.append(i.string.encode('utf-8'))
        soup.html.body.center.h1.contents
        regmajor = re.compile('[ \n]+$')
        major = regmajor.split(soup.html.body.center.h1.contents[0])[0].encode('ISO-8859-1')
        faculty = regmajor.split(soup.html.body.center.h1.contents[2])[0].encode('ISO-8859-1')
        i=1
        folio = re.compile('([0-9]+) +([A-Z])')
        a = re.compile('([0-9]+) +(A)')
        while i<len(data):
          m = folio.search(data[i])
          if m :
            row = [m.group(1),m.group(2),'R',major,faculty]
            writer.writerow(row)
            print 'yes'
            i=i+1
          else:
            if i<len(data)-1:
              m = a.search(data[i+1])
              if m:
                row = [data[i],m.group(1).encode('utf-8'),m.group(2).encode('utf-8'),major,faculty]
                writer.writerow(row)
              else:
                row = [data[i],data[i+1],'R',major,faculty]
                writer.writerow(row)
              i=i+2
            else:
              i=i+2
        time.sleep(20)
    return

results=open('results.csv','wb')
writer = csv.writer(results,dialect='excel')

#'https://servicios.dgae.unam.mx/Febrero2009/resultados/15.html'
#
#'https://servicios.dgae.unam.mx/Febrero2009/resultados/35.html'
#'https://servicios.dgae.unam.mx/Febrero2009/resultados/45.html'
url='https://servicios.dgae.unam.mx/Febrero2009/resultados/25.html'
request = urllib2.Request( url )
page = urllib2.urlopen( request )
soup = BeautifulSoup(page)
majors=soup.findAll('a',href=re.compile("^[1234]/*"))
for i in majors:
  print i['href']

get_data(majors)

results.close