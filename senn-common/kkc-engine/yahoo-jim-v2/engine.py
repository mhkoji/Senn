#!/usr/bin/env python3

import json
import urllib.request
import sys

class YahooClient:
    def __init__(self, appId):
        self.appId = appId

    def convert(self, q):
        URL = "https://jlp.yahooapis.jp/JIMService/V2/conversion"
        headers = {
            "Content-Type": "application/json",
            "User-Agent": "Yahoo AppID: {}".format(self.appId),
        }
        param_dic = {
            "id": "1234-1",
            "jsonrpc": "2.0",
            "method": "jlp.jimservice.conversion",
            "params": {
                "q": q,
                "format": "hiragana",
                "mode": "kanakanji",
                "option": ["hiragana", "katakana", "alphanumeric", "half_katakana", "half_alphanumeric"],
                "dictionary": ["base", "name", "place", "zip", "symbol"],
                "results": 999
            }
        }
        params = json.dumps(param_dic).encode()
        req = urllib.request.Request(URL, params, headers)

        dict_resp = None
        with urllib.request.urlopen(req) as res:
            dict_resp = json.loads(res.read().decode())

        return dict_resp['result']['segment']

def server(yahoo_client):
    for line in sys.stdin:
        req = json.loads(line)
        op = req['op']

        if op == 'CONVERT':
            pron = req['args']['pron']
            segment = yahoo_client.convert(pron)

            candidates_list = []
            array = []
            for seg in segment:
                pron = seg['reading']
                candidates = []
                for form in seg['candidate']:
                    candidates.append({
                        'form': form
                    })
                array.append({
                    'pron': pron,
                    'candidates': candidates,
                })
            resp = json.dumps(array)
            sys.stdout.write(resp + '\n')
            sys.stdout.flush()

        elif op == 'LIST_CANDIDATES':
            resp = json.dumps([])
            sys.stdout.write(resp + '\n')
            sys.stdout.flush()

        else:
            raise AssertionError()

if __name__ == '__main__':
    APPID = "" ## Your appId here
    yahoo_client = YahooClient(APPID)
    server(yahoo_client)
