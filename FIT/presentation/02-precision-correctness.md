##

```haskell
foo :: [Int] -> [Int]
foo = _
```

vs.

```haskell
foo :: Functor f => f Int -> f Int
foo = _
```

vs.
```haskell
foo :: a -> a
foo = id
```

##

But abstraction effectively *constrains* the number of primitives
we can use

* Less primitives reduces the number of possible implementations
* Less possible implementations => less incorrect implementations

##

```haskell
cata :: Tree Int -> Int
cata = _
```

vs.

```haskell
cata :: Foldable f => f Int -> Int
cata = _
```

##

Concrete things do **NOT** compose!

##

```haskell
whatDoesItDo :: (
  ConnectionDB f,
  MonadReader Config f
) => IO ()
```

vs.

```haskell
whatDoesItDo :: Any -> ()
```

##

**Maximum** power
\
**Minimal** amount of reasoning

##

> Simplicity is a great virtue but it requires hard work
> to achieve it and education to appreciate it. And to make
> matters worse: complexity sells better.
>
>   -- E. W. Dijkstra
