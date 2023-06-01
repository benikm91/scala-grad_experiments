# ScalaGrad 

## QuickStart

### Minimal Example

```scala mdoc
import scalagrad.api.ScalaGrad.derive
import spire.math.Numeric
import spire.implicits._

def f[T: Numeric](x: T): T = 
    x * x

// goal API
// import ScalaGrad.default.given  // default exports best default implementations
// val df = derive[Double](f)  // goal

// current API
import scalagrad.auto.forward.dual.DualNumber
import scalagrad.auto.forward.dual.DualNumber.given
import scalagrad.auto.forward.DeriverForwardPlan.given

val df = derive(f[DualNumber[Double]])  // currently

val x = 5
assert(df(x) == 2 * x)
```

### Specific Mode

TODO

### Higher Order

```scala mdoc
// current API

def f1[T](x: T)(using f: Fractional[T]): T = 
    import f._
    x * x

val ddf = ScalaGrad.derive(ScalaGrad.derive(f1[DualNumber[DualDelta[Double]]]))
println(ddf(5.0))
```

### Vector Example

```

def relu[P](x: P)(using num: Numeric[P]) = 
    if x <= num.zero then 
        num.zero 
    else 
        x

def sigmoid[P](x: P) = ???

def f[LA <: LinearAlgebra](input: LA.Vector, w1: LA.Matrix, w2: LA.Vector)(using o: LinearAlgebraOps[LA]): LA.Scalar = 
    val hIn = o.dot(input, w1)
    val hOut = o.elementWise(hIn, relu)
    val h2In = o.dot(w2, hOut)
    val h2Out = sigmoid(h2In)
    h2Out

```