# big-step

```bash
$ ghci Evaluation.hs
```

```haskell
> :l Tests.hs
> putStr $ vizExecution progMin
```

```haskell
🤜 Assign "a" (NLit 1.0)
      🤜 NLit 1.0
      🥊 1.0
🥊 ()

🤜 Assign "b" (NLit 24.0)
      🤜 NLit 24.0
      🥊 24.0
🥊 ()

🤜 If (BOp (ValOf "a") < (ValOf "b")) [Assign "min" (ValOf "a")] [Assign "min" (ValOf "b")]
      🤜 BOp (ValOf "a") < (ValOf "b")
            🤜 ValOf "a"
            🥊 1.0
            🤜 ValOf "b"
            🥊 24.0
      🥊 True
      🤜 Assign "min" (ValOf "a")
            🤜 ValOf "a"
            🥊 1.0
      🥊 ()

🥊 ()
```