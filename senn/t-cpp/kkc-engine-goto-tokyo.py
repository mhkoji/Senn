#!/usr/bin/env python3
import sys
import json

def main():
    for line in sys.stdin:
        req = json.loads(line)
        op = req["op"]

        if op == "CONVERT":
            pron = req["args"]["pron"]
            assert pron == "とうきょうにいきました"

            resp = [{
                "pron": "とうきょう",
                "candidates": [{ "form": "東京" }]
            }, {
                "pron": "に",
                "candidates": [{ "form": "に" }]
            }, {
                "pron": "い",
                "candidates": [{ "form": "行" }]
            }, {
                "pron": "き",
                "candidates": [{ "form": "き" }]
            }, {
                "pron": "ま",
                "candidates": [{ "form": "ま" }]
            }, {
                "pron": "し",
                "candidates": [{ "form": "し" }]
            }, {
                "pron": "た",
                "candidates": [{ "form": "た" }]
            }]
            sys.stdout.write(json.dumps(resp) + "\n")
            sys.stdout.flush()
        else:
            raise AssertionError()

if __name__ == "__main__":
    main()
