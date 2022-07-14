## Spam-Ham filtering in haskell

### Running
- use the `classifyFolder` function. Pass it a `SpamModel`
   - example:
      ```sh
      cabal repl
      ...
      > sm <- spamModel
      > classifyFolder sm "./data/validate/ham"
      ```
     - Output format:
         ```
         path/to/file/classifed -> (ps, ph)
         ```

       where: `ps = spam probability` and `ph = ham probability`
   
