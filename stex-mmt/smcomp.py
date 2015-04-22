#!/usr/bin/python

import stomp
import urllib
import glob
import os
import time
import sys
import threading
import json

repo = sys.argv[1]
SRC = "/var/data/localmh/MathHub/smglom/%s/source"%repo;
DIR = "*.tex";
LATEX_QUEUE = "/queue/latexml";
RESULT_QUEUE = "/queue/asd";

conn = stomp.Connection()
conn.start()
conn.connect("admin", "admin")

start_time = time.mktime(time.localtime())

sent = 0
rec = 0
done = 0

class MyListener(object):
    def on_error(self, headers, message):
        print('received an error %s' % message)
    def on_message(self, headers, message):
        global rec
        global sent
        global start_time
        global done

        rec = rec+1
        ctime = time.mktime(time.localtime())

        msg = json.JSONDecoder().decode(message)

        logfile = open(headers["log"], "w");
        logfile.write(msg["log"]);
        logfile.close();

        print("Received %s/%s %f sec/request : %s\n%s"%(rec, sent, (ctime-start_time)/rec, headers["file"], msg["status"]))
        if (rec == sent):
            done = 1

conn.set_listener('', MyListener())

conn.subscribe(destination=RESULT_QUEUE, id=1, ack='auto')

for f in glob.glob(SRC+"/"+DIR):
    fileName, fileExtension = os.path.splitext(f)
    if "all." in f:
        continue;
    msg = {
        "profile": "stex-smglom-module",
        "path" : "/var/data/localmh/sty",
        "source" : f,
        "quiet" : "",
        "destination" : fileName+".omdoc",
        "log" : fileName+".ltxlog",
        "preamble" : "/var/data/localmh/MathHub/smglom/%s/lib/pre.de.tex"%repo,
        "postamble" : "/var/data/localmh/MathHub/smglom/%s/lib/post.de.tex"%repo,
        "base" : SRC,
    };

    headers = {"reply-to":RESULT_QUEUE, "correlation-id" : "1", "file" : f, "log" : fileName+".ltxlog"};

    conn.send(LATEX_QUEUE, urllib.urlencode(msg), headers=headers)
    sent=sent+1

while done == 0:
    time.sleep(1);
conn.disconnect()
