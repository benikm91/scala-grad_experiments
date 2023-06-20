package scalagrad.linearalgebra.auto.reverse.delta.visualize

import scalagrad.linearalgebra.auto.reverse.delta.*


object DeltaTraverse:

    def traverse[P](d: Deltas[P]): LazyList[Deltas[P]] = 
        d match {
            case dcv: DeltaColumnVector[P] => traverseColumnVector(dcv)
            case drv: DeltaRowVector[P] => traverseRowVector(drv)
            case ds: DeltaScalar[P] => traverseScalar(ds)
            case dm: DeltaMatrix[P] => traverseMatrix(dm)
        }

    def traverseScalar[P](ds: DeltaScalar[P]): LazyList[Deltas[P]] =
        ds match {
            case z: DeltaScalar.Zero[P] => LazyList(z)
            case l: DeltaScalar.Let[P] => l #:: (traverse(l.rhs) ++ traverseScalar(l.body))
            case v: DeltaScalar.Val[P] => LazyList(v)
            case m: DeltaScalar.MultiplyVV[P] => m #:: (traverseRowVector(m.m1) ++ traverseColumnVector(m.m2))
            case m: DeltaScalar.MultiplyRVDCV[P] => m #:: traverseColumnVector(m.d)
            case d: DeltaScalar.MatrixDotDRVCV[P] => d #:: traverseRowVector(d.d)
            case c: DeltaScalar.ColumnDotProduct[P] => c #:: traverseColumnVector(c.d)
            case r: DeltaScalar.RowDotProduct[P] => r #:: traverseRowVector(r.d)
            case a: DeltaScalar.Add[P] => a #:: (traverseScalar(a.s1) ++ traverseScalar(a.s2))
            case s: DeltaScalar.Sub[P] => s #:: (traverseScalar(s.s1) ++ traverseScalar(s.s2))
            case d: DeltaScalar.Div[P] => d #:: traverseScalar(d.s1)
            case s: DeltaScalar.Scale[P] => s #:: traverseScalar(s.ds)
            case s: DeltaScalar.Sum[P] => s #:: traverseColumnVector(s.v)
            case s: DeltaScalar.SumM[P] => s #:: traverseMatrix(s.m)
            case e: DeltaScalar.ElementAtM[P] => e #:: traverseMatrix(e.m)
            case e: DeltaScalar.ElementAtCV[P] => e #:: traverseColumnVector(e.delta)
            case e: DeltaScalar.ElementAtRV[P] => e #:: traverseRowVector(e.delta)
        }

    def traverseColumnVector[P](dcv: DeltaColumnVector[P]): LazyList[Deltas[P]] = 
        dcv match {
            case z: DeltaColumnVector.Zero[P] => LazyList(z)
            case v: DeltaColumnVector.Val[P] => LazyList(v)
            case l: DeltaColumnVector.Let[P] => l #:: (traverse(l.rhs) ++ traverseColumnVector(l.body))
            case t: DeltaColumnVector.Transpose[P] => t #:: traverseRowVector(t.v)
            case m1: DeltaColumnVector.MatrixDotDMCV[P] => m1 #:: traverseMatrix(m1.d)
            case m2: DeltaColumnVector.MatrixDotMDCV[P] => m2 #:: traverseColumnVector(m2.d)
            case m3: DeltaColumnVector.MatrixDotCVDS[P] => m3 #:: traverseScalar(m3.ds)
            case m4: DeltaColumnVector.MatrixDotDCVS[P] => m4 #:: traverseColumnVector(m4.d)
            case d: DeltaColumnVector.Div[P] => d #:: traverseColumnVector(d.d)
            case a1: DeltaColumnVector.AddDCVDCV[P] => a1 #:: (traverseColumnVector(a1.d1) ++ traverseColumnVector(a1.d2))
            case a2: DeltaColumnVector.AddVS[P] => a2 #:: (traverseColumnVector(a2.d) ++ traverseScalar(a2.s))
            case m1: DeltaColumnVector.MinusVV[P] => m1 #:: (traverseColumnVector(m1.d1) ++ traverseColumnVector(m1.d2))
            case m2: DeltaColumnVector.MinusVS[P] => m2 #:: (traverseColumnVector(m2.d) ++ traverseScalar(m2.s))
            case e1: DeltaColumnVector.ElementWiseScale[P] => e1 #:: traverseColumnVector(e1.d)
            case e2: DeltaColumnVector.FromElements[P] => e2 #:: e2.values.to(LazyList).flatMap(traverseScalar)
            case e3: DeltaColumnVector.ElementWiseOps[P] => e3 #:: traverseColumnVector(e3.d)
        }

    def traverseRowVector[P](drv: DeltaRowVector[P]): LazyList[Deltas[P]] = 
          drv match {
            case z: DeltaRowVector.Zero[P] => LazyList(z)
            case l: DeltaRowVector.Let[P] => l #:: (traverse(l.rhs) ++ traverseRowVector(l.body))
            case v: DeltaRowVector.Val[P] => LazyList(v)
            case t: DeltaRowVector.Transpose[P] => t #:: traverseColumnVector(t.v)
            case s: DeltaRowVector.Scale[P] => s #:: traverseRowVector(s.drv)
            case m: DeltaRowVector.MatrixDotRVDM[P] => m #:: traverseMatrix(m.d)
            case m: DeltaRowVector.MatrixDotDRVM[P] => m #:: traverseRowVector(m.d)
            case m: DeltaRowVector.MatrixDotRVDS[P] => m #:: traverseScalar(m.ds)
            case a: DeltaRowVector.AddVV[P] => a #:: (traverseRowVector(a.d1) ++ traverseRowVector(a.d2))
            case a: DeltaRowVector.AddDRVDS[P] => a #:: (traverseRowVector(a.drv) ++ traverseScalar(a.ds))
            case m: DeltaRowVector.MinusDRVDRV[P] => m #:: (traverseRowVector(m.drv1) ++ traverseRowVector(m.drv2))
            case m: DeltaRowVector.MinusDRVDS[P] => m #:: (traverseRowVector(m.drv) ++ traverseScalar(m.ds))
            case e: DeltaRowVector.ElementWiseScale[P] => e #:: traverseRowVector(e.d)
            case d: DeltaRowVector.Div[P] => d #:: traverseRowVector(d.drv)
            case r: DeltaRowVector.RowAtM[P] => r #:: traverseMatrix(r.d)
            case f: DeltaRowVector.FromElements[P] => f #:: f.values.to(LazyList).flatMap(traverseScalar)
            case e: DeltaRowVector.ElementWiseOps[P] => e #:: traverseRowVector(e.d)
        }

    def traverseMatrix[P](dm: DeltaMatrix[P]): LazyList[Deltas[P]] =
        dm match {
            case z: DeltaMatrix.Zero[P] => LazyList(z)
            case v: DeltaMatrix.Val[P] => LazyList(v)
            case l: DeltaMatrix.Let[P] => l #:: (traverse(l.rhs) ++ traverseMatrix(l.body))
            case t: DeltaMatrix.Transpose[P] => t #:: traverseMatrix(t.m)
            case md: DeltaMatrix.MatrixDotMDM[P] => md #:: traverseMatrix(md.d)
            case md: DeltaMatrix.MatrixDotDMM[P] => md #:: traverseMatrix(md.d)
            case md: DeltaMatrix.MatrixDotMDS[P] => md #:: traverseScalar(md.s)
            case md: DeltaMatrix.MatrixDotDMS[P] => md #:: traverseMatrix(md.d)
            case md: DeltaMatrix.MatrixDotDCVRV[P] => md #:: traverseColumnVector(md.d)
            case md: DeltaMatrix.MatrixDotCVDRV[P] => md #:: traverseRowVector(md.d)
            case a: DeltaMatrix.AddDMDM[P] => a #:: (traverseMatrix(a.m1) ++ traverseMatrix(a.m2))
            case a: DeltaMatrix.AddDMDS[P] => a #:: (traverseMatrix(a.m) ++ traverseScalar(a.s))
            case a: DeltaMatrix.AddDMDRV[P] => a #:: (traverseMatrix(a.m) ++ traverseRowVector(a.v))
            case a: DeltaMatrix.AddDMDCV[P] => a #:: (traverseMatrix(a.m) ++ traverseColumnVector(a.v))
            case m: DeltaMatrix.MinusDMDM[P] => m #:: (traverseMatrix(m.dm1) ++ traverseMatrix(m.dm2))
            case m: DeltaMatrix.MinusDMDCV[P] => m #:: (traverseMatrix(m.dm) ++ traverseColumnVector(m.dcv))
            case m: DeltaMatrix.MinusDMDRV[P] => m #:: (traverseMatrix(m.dm) ++ traverseRowVector(m.drv))
            case m: DeltaMatrix.MinusDMDS[P] => m #:: (traverseMatrix(m.dm) ++ traverseScalar(m.ds))
            case d: DeltaMatrix.Div[P] => d #:: traverseMatrix(d.m)
            case e: DeltaMatrix.ElementWiseScale[P] => e #:: traverseMatrix(e.d)
            case f: DeltaMatrix.FromElements[P] => f #:: f.values.to(LazyList).flatMap(traverseScalar)
            case s: DeltaMatrix.StackDRows[P] => s #:: s.rows.to(LazyList).flatMap(traverseRowVector)
            case e: DeltaMatrix.ElementWiseOps[P] => e #:: traverseMatrix(e.d)
            case c: DeltaMatrix.ColumnWiseOps[P] => c #:: traverseMatrix(c.d)
            case r: DeltaMatrix.RowWiseOps[P] => r #:: traverseMatrix(r.d)
            case e: DeltaMatrix.ElementWiseOpsForward[P] => e #:: traverseMatrix(e.d)
            case r: DeltaMatrix.RowWiseOpsForward[P] => r #:: traverseMatrix(r.d)
            case r: DeltaMatrix.RowWiseOpsManual[P] => r #:: traverseMatrix(r.d)
        }

