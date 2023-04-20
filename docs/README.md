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

// curernt API
import scalagrad.auto.forward.dual.DualNumber
import scalagrad.auto.forward.dual.DualNumber.given
import scalagrad.auto.forward.DeriverForwardPlan.given

val df = derive(f[DualNumber[Double]])  // currently

val x = 5
assert(df(x) == 2 * x)
```

### Specific Mode

TODO