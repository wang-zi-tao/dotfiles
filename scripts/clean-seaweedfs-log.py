#! /usr/bin/env nix-shell
#!nix-shell -i python3 --packages python3 python3Packages.requests
import requests
from datetime import datetime, timedelta,time
r = requests.get("http://localhost:302/topics/.system/log/",headers={"Accept":"application/json"})
deadline = datetime.now()-timedelta(days=7)
for item in r.json()["Entries"]:
    modify_time = datetime.fromisoformat(item["Mtime"])
    if modify_time.timestamp()<deadline.timestamp():
        path = item["FullPath"]
        print("delete: ",path)
        r=requests.delete("http://localhost:302"+path,params={"recursive":"true"})
        print(r)
