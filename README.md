# Senn Input Method

Input method editor for the Japanese language

## KKC Engine

- The KKC of the ime is implemented in the separated file kkc-engine.
- The kkc-engine file is installed at `/usr/lib/senn/fcitx/kkc-engine`.
- The kkc-engine file can be replaced with any executable file as long as it satisfies the protocol described below to communication with the ime.

### The protocol

A kkc-engine receives a request of a line from the standard input and sends a response of a line to the standard output.
A line of the requests and responses is JSON encoded.

Each json form of requests is as follows:
```json
{ "op": "<request op>", "args": "<request args>" }
```

The json form of a response depends on the request.


### CONVERT request

A Convert request is used to converts a sequence that the user inputs to a word sequence.

Request
- `op`: "CONVERT"
- `args`:
  - `pron`: <string>
    - A string of the sequence (typically hiragana)

Response
- array of objects:   // An array of objects that describes a word sequence
  - `form`: <string>  // A string of kana and kanji for the word
  - `pron`: <string>  // A substring of the input sequence for the word


### LOOKUP request

A LOOKUP request is used to lookup more candidates for a word from CONVERT.

Request
- `op`: "LOOKUP"
- `args`:
  - `pron`: <string>
    - A substring of the word

Response
- array of objects:   // An array of objects that describes candidates
  - `form`: <string>  // A string of kana and kanji

