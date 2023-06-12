package scalagrad.showcase.deeplearning


import scala.io.Source

object MNISTDataSet:

    case class MNIST(
        image: Vector[Vector[Double]],
    )

    case class MNISTEntry(
        label: String,
        pixels: Seq[Double]
    )

    def load: Iterator[MNISTEntry] = 
        val content = Source.fromFile("showcases/showcase-deep-learning/res/mnist/mnist_train.csv")
            .getLines.map(_.split(","))
        content.drop(1).map(row =>
            MNISTEntry(
                row.head,
                row.tail.map(_.toDouble)
            )
        )