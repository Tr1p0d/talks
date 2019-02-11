## 

![](./img/the-holy-trinity-pythagoras-2.png)

## 

![](./img/the-holy-trinity-pythagoras-1.png)

##

```scala

def someFun(n: Int): Int = ???

```

vs.

```scala

def someFun[A](a: A): A = a

```

##

```scala

def someFun[F[_]: Applicative, A]: F[A] = ???

```

vs.

```scala

def someFun[F[_]: Monad, A]: F[A] = ???

```

##

Magic constants aka concretising way too early:

~~~scala

def effectfulFun[A]: Future[A] = ???

~~~

vs.

```scala

def effectfulFun[F[_]: Effect, A]: F[A] = ???

```

Does **NOT** compose!

##

* Maximum power
* Minimal amount of reasoning

```scala

def someFun[F[_]: Effect](a: A): F[B] = ???

```

vs.

```scala

def receive: Any => Unit = ???

```

If you constrain yourself from power, you get (not only) reasoning back.

##

There is no such thing as correct code.

But abstraction effectively *constrains* the number of primitives
we can use.

* Less primitives reduces the number of possible implementations
* Less possible implementations => less incorrect implementations

## 

FP allows for reasoning by constrains

* Totality
* Pureness
* Lack of side effects

You get the toolbox for free


##

> Simplicity is a great virtue but it requires hard work 
> to achieve it and education to appreciate it. And to make 
> matters worse: complexity sells better.
>
>   -- E. W. Dijkstra