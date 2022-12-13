#!/usr/bin/env python3
import sys
import json

def main():
    for line in sys.stdin:
        req = json.loads(line)
        op = req["op"]

        if op == "CONVERT":
            pron = req["args"]["pron"]
            assert pron == "きょうは"

            resp = [{
                "pron": "きょう",
                "candidates": [{ "form": "今日" }]
            }, {
                "pron": "は",
                "candidates": [{ "form": "は" }]
            }]
            sys.stdout.write(json.dumps(resp) + "\n")
            sys.stdout.flush()
        else:
            raise AssertionError()

if __name__ == "__main__":
    main()
