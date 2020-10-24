# frameit-mmt: Server component of FrameIT project

This is the server component of the [FrameIT project](https://kwarc.info/systems/frameit/), primarily maintained so far by [@ComFreek](https://github.com/ComFreek).

## Installation & Compilation

See [./installation.md](./installation.md).

## REST API

- <details><summary>Fact reference JSON format (shared by endpoints below)</summary>

    ```javascript
    {"uri": /* some uri */}
    ```
    
    Format only given for informational purposes, the game engine should treat fact reference objects opaquely.
    Do not depend on their internal structure.

  </details>

- <details><summary>Fact JSON format (shared by endpoints below)</summary>

    - variant a: general facts:
    
      ```javascript
      {
        "ref": /* fact reference */
        "label": "some label",
        "kind": "general",
        "tp": /* OMDoc JSON term */,
        "df": /* OMDoc JSON term or null or left out */
      }
      ```

    - variant b: veq facts
    
      ```javascript
      {
        "ref": /* fact reference */
        "label": "some label",
        "kind": "veq",
        "lhs":   /* OMDoc JSON term */,
        "value": /* OMF OmDoc JSON term */
      }
      ```

  </details>

- <details><summary>Scroll reference JSON format (shared by endpoints below)</summary>

    ```javascript
    {
      "problemTheory": /* MMT URI as JSON string */,
      "solutionTheory": /* MMT URI as JSON string */
    }
    ```
  
    Format only given for informational purposes, the game engine should treat scroll reference objects opaquely.
    Do not depend on their internal structure. 

  </details>

- <details><summary>Scroll JSON format (shared by endpoints below)</summary>

    ```javascript
    {
      "ref": /* scroll reference */,
      "label": "some label",
      "description": "some description",
      "requiredFacts": /* array of facts */
    }
    ```

  </details>

- <details><summary>Scroll Application format (shared by endpoints below)</summary>

    ```javascript
    {
      "scroll": /* scroll reference */,
      "assignments": [
        ["ref": /* fact reference */, /* OMDoc JSON term (the assignment) */],
        /* ... more elements (same syntax) */
      ]
    }
    ```

  </details>

- <details><summary><code>POST /fact/add</code>: make a new fact known to the server</summary>

  - payload: a fact JSON object as detailled above without the "ref" field
  - return value: a fact reference JSON object

  </details>

- <details><summary><code>GET /fact/list</code>: retrieve all facts known to the server</summary>

  - payload: none
  - return value: a JSON array containing fact JSON objects

  </details>

- <details><summary><code>GET /scroll/list</code>: retrieve all scrolls known to the server</summary>

  - payload: none
  - return value: a JSON array containing scroll JSON objects

  </details>

- <details><summary><code>POST /scroll/apply</code>: apply (i.e. use) a scroll and add acquired facts to situation theory</summary>

  - payload: none
  - return value: a JSON array containing scroll JSON objects
  </details>


## Internal REST API

- <details><summary><code>GET /debug/situationtheory/print</code>: output a stringification of the situation theory (and included theories) known to the server</summary>

  - payload: none
  - return value: a JSON string containing MMT surface syntax (probably unparsable by MMT; for human consumption only)

  </details>

## License

Same as the whole MMT source code with the exception of a snippet copied from "the Internet" into `src/info/kwarc/mmt/frameit/communication/ServerErrorHandler.scala`. The precise source and license is given there.
