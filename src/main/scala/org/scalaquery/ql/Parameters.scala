package org.scalaquery.ql

import java.sql.PreparedStatement
import org.scalaquery.SQueryException
import org.scalaquery.ql.basic.{BasicProfile, BasicQueryTemplate}

final class Parameters[P, C](c: C) {
  def flatMap[F](f: C => Query[ColumnBase[F]])(implicit profile: BasicProfile): BasicQueryTemplate[P, F] =
    profile.createQueryTemplate[P, F](f(c))
  def map[F](f: C => ColumnBase[F])(implicit profile: BasicProfile): BasicQueryTemplate[P, F] =
    profile.createQueryTemplate[P, F](Query(f(c)))
  def filter(f: C => Boolean): Parameters[P, C] =
    if(!f(c)) throw new SQueryException("Match failed when unpacking Parameters")
    else this
}

object Parameters {
  def apply[P1 : TypeMapper] = new Parameters[P1, Column[P1]](new ParameterColumn(-1))

  def apply[P1 : TypeMapper, P2 : TypeMapper] =
    new Parameters[(P1,P2), Projection2[P1,P2]](new Projection2(
    new ParameterColumn[P1](0),
    new ParameterColumn[P2](1)
  ))

  def apply[P1 : TypeMapper, P2 : TypeMapper, P3 : TypeMapper] =
    new Parameters[(P1,P2,P3), Projection3[P1,P2,P3]](new Projection3(
    new ParameterColumn[P1](0),
    new ParameterColumn[P2](1),
    new ParameterColumn[P3](2)
  ))

  def apply[P1 : TypeMapper, P2 : TypeMapper, P3 : TypeMapper, P4 : TypeMapper] =
    new Parameters[(P1,P2,P3,P4), Projection4[P1,P2,P3,P4]](new Projection4(
    new ParameterColumn[P1](0),
    new ParameterColumn[P2](1),
    new ParameterColumn[P3](2),
    new ParameterColumn[P4](3)
  ))

  def apply[P1 : TypeMapper, P2 : TypeMapper, P3 : TypeMapper, P4 : TypeMapper, P5 : TypeMapper] =
    new Parameters[(P1,P2,P3,P4,P5), Projection5[P1,P2,P3,P4,P5]](new Projection5(
    new ParameterColumn[P1](0),
    new ParameterColumn[P2](1),
    new ParameterColumn[P3](2),
    new ParameterColumn[P4](3),
    new ParameterColumn[P5](4)
  ))

  def apply[P1 : TypeMapper, P2 : TypeMapper, P3 : TypeMapper, P4 : TypeMapper, P5 : TypeMapper, P6 : TypeMapper] =
    new Parameters[(P1,P2,P3,P4,P5,P6), Projection6[P1,P2,P3,P4,P5,P6]](new Projection6(
    new ParameterColumn[P1](0),
    new ParameterColumn[P2](1),
    new ParameterColumn[P3](2),
    new ParameterColumn[P4](3),
    new ParameterColumn[P5](4),
    new ParameterColumn[P6](5)
  ))

  def apply[P1 : TypeMapper, P2 : TypeMapper, P3 : TypeMapper, P4 : TypeMapper, P5 : TypeMapper, P6 : TypeMapper, P7 : TypeMapper] =
    new Parameters[(P1,P2,P3,P4,P5,P6,P7), Projection7[P1,P2,P3,P4,P5,P6,P7]](new Projection7(
    new ParameterColumn[P1](0),
    new ParameterColumn[P2](1),
    new ParameterColumn[P3](2),
    new ParameterColumn[P4](3),
    new ParameterColumn[P5](4),
    new ParameterColumn[P6](5),
    new ParameterColumn[P7](6)
  ))

  def apply[P1 : TypeMapper, P2 : TypeMapper, P3 : TypeMapper, P4 : TypeMapper, P5 : TypeMapper, P6 : TypeMapper, P7 : TypeMapper, P8 : TypeMapper] =
    new Parameters[(P1,P2,P3,P4,P5,P6,P7,P8), Projection8[P1,P2,P3,P4,P5,P6,P7,P8]](new Projection8(
    new ParameterColumn[P1](0),
    new ParameterColumn[P2](1),
    new ParameterColumn[P3](2),
    new ParameterColumn[P4](3),
    new ParameterColumn[P5](4),
    new ParameterColumn[P6](5),
    new ParameterColumn[P7](6),
    new ParameterColumn[P8](7)
  ))

  def apply[P1 : TypeMapper, P2 : TypeMapper, P3 : TypeMapper, P4 : TypeMapper, P5 : TypeMapper, P6 : TypeMapper, P7 : TypeMapper, P8 : TypeMapper, P9 : TypeMapper] =
    new Parameters[(P1,P2,P3,P4,P5,P6,P7,P8,P9), Projection9[P1,P2,P3,P4,P5,P6,P7,P8,P9]](new Projection9(
    new ParameterColumn[P1](0),
    new ParameterColumn[P2](1),
    new ParameterColumn[P3](2),
    new ParameterColumn[P4](3),
    new ParameterColumn[P5](4),
    new ParameterColumn[P6](5),
    new ParameterColumn[P7](6),
    new ParameterColumn[P8](7),
    new ParameterColumn[P9](8)
  ))

  def apply[P1 : TypeMapper, P2 : TypeMapper, P3 : TypeMapper, P4 : TypeMapper, P5 : TypeMapper, P6 : TypeMapper, P7 : TypeMapper, P8 : TypeMapper, P9 : TypeMapper, P10 : TypeMapper] =
    new Parameters[(P1,P2,P3,P4,P5,P6,P7,P8,P9,P10), Projection10[P1,P2,P3,P4,P5,P6,P7,P8,P9,P10]](new Projection10(
    new ParameterColumn[P1](0),
    new ParameterColumn[P2](1),
    new ParameterColumn[P3](2),
    new ParameterColumn[P4](3),
    new ParameterColumn[P5](4),
    new ParameterColumn[P6](5),
    new ParameterColumn[P7](6),
    new ParameterColumn[P8](7),
    new ParameterColumn[P9](8),
    new ParameterColumn[P10](9)
  ))

  def apply[P1 : TypeMapper, P2 : TypeMapper, P3 : TypeMapper, P4 : TypeMapper, P5 : TypeMapper, P6 : TypeMapper, P7 : TypeMapper, P8 : TypeMapper, P9 : TypeMapper, P10 : TypeMapper, P11 : TypeMapper] =
    new Parameters[(P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11), Projection11[P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11]](new Projection11(
    new ParameterColumn[P1](0),
    new ParameterColumn[P2](1),
    new ParameterColumn[P3](2),
    new ParameterColumn[P4](3),
    new ParameterColumn[P5](4),
    new ParameterColumn[P6](5),
    new ParameterColumn[P7](6),
    new ParameterColumn[P8](7),
    new ParameterColumn[P9](8),
    new ParameterColumn[P10](9),
    new ParameterColumn[P11](10)
  ))

  def apply[P1 : TypeMapper, P2 : TypeMapper, P3 : TypeMapper, P4 : TypeMapper, P5 : TypeMapper, P6 : TypeMapper, P7 : TypeMapper, P8 : TypeMapper, P9 : TypeMapper, P10 : TypeMapper, P11 : TypeMapper, P12 : TypeMapper] =
    new Parameters[(P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12), Projection12[P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12]](new Projection12(
    new ParameterColumn[P1](0),
    new ParameterColumn[P2](1),
    new ParameterColumn[P3](2),
    new ParameterColumn[P4](3),
    new ParameterColumn[P5](4),
    new ParameterColumn[P6](5),
    new ParameterColumn[P7](6),
    new ParameterColumn[P8](7),
    new ParameterColumn[P9](8),
    new ParameterColumn[P10](9),
    new ParameterColumn[P11](10),
    new ParameterColumn[P12](11)
  ))

  def apply[P1 : TypeMapper, P2 : TypeMapper, P3 : TypeMapper, P4 : TypeMapper, P5 : TypeMapper, P6 : TypeMapper, P7 : TypeMapper, P8 : TypeMapper, P9 : TypeMapper, P10 : TypeMapper, P11 : TypeMapper, P12 : TypeMapper, P13 : TypeMapper] =
    new Parameters[(P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13), Projection13[P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13]](new Projection13(
    new ParameterColumn[P1](0),
    new ParameterColumn[P2](1),
    new ParameterColumn[P3](2),
    new ParameterColumn[P4](3),
    new ParameterColumn[P5](4),
    new ParameterColumn[P6](5),
    new ParameterColumn[P7](6),
    new ParameterColumn[P8](7),
    new ParameterColumn[P9](8),
    new ParameterColumn[P10](9),
    new ParameterColumn[P11](10),
    new ParameterColumn[P12](11),
    new ParameterColumn[P13](12)
  ))

  def apply[P1 : TypeMapper, P2 : TypeMapper, P3 : TypeMapper, P4 : TypeMapper, P5 : TypeMapper, P6 : TypeMapper, P7 : TypeMapper, P8 : TypeMapper, P9 : TypeMapper, P10 : TypeMapper, P11 : TypeMapper, P12 : TypeMapper, P13 : TypeMapper, P14 : TypeMapper] =
    new Parameters[(P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14), Projection14[P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14]](new Projection14(
    new ParameterColumn[P1](0),
    new ParameterColumn[P2](1),
    new ParameterColumn[P3](2),
    new ParameterColumn[P4](3),
    new ParameterColumn[P5](4),
    new ParameterColumn[P6](5),
    new ParameterColumn[P7](6),
    new ParameterColumn[P8](7),
    new ParameterColumn[P9](8),
    new ParameterColumn[P10](9),
    new ParameterColumn[P11](10),
    new ParameterColumn[P12](11),
    new ParameterColumn[P13](12),
    new ParameterColumn[P14](13)
  ))

  def apply[P1 : TypeMapper, P2 : TypeMapper, P3 : TypeMapper, P4 : TypeMapper, P5 : TypeMapper, P6 : TypeMapper, P7 : TypeMapper, P8 : TypeMapper, P9 : TypeMapper, P10 : TypeMapper, P11 : TypeMapper, P12 : TypeMapper, P13 : TypeMapper, P14 : TypeMapper, P15 : TypeMapper] =
    new Parameters[(P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14,P15), Projection15[P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14,P15]](new Projection15(
    new ParameterColumn[P1](0),
    new ParameterColumn[P2](1),
    new ParameterColumn[P3](2),
    new ParameterColumn[P4](3),
    new ParameterColumn[P5](4),
    new ParameterColumn[P6](5),
    new ParameterColumn[P7](6),
    new ParameterColumn[P8](7),
    new ParameterColumn[P9](8),
    new ParameterColumn[P10](9),
    new ParameterColumn[P11](10),
    new ParameterColumn[P12](11),
    new ParameterColumn[P13](12),
    new ParameterColumn[P14](13),
    new ParameterColumn[P15](14)
  ))

  def apply[P1 : TypeMapper, P2 : TypeMapper, P3 : TypeMapper, P4 : TypeMapper, P5 : TypeMapper, P6 : TypeMapper, P7 : TypeMapper, P8 : TypeMapper, P9 : TypeMapper, P10 : TypeMapper, P11 : TypeMapper, P12 : TypeMapper, P13 : TypeMapper, P14 : TypeMapper, P15 : TypeMapper, P16 : TypeMapper] =
    new Parameters[(P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14,P15,P16), Projection16[P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14,P15,P16]](new Projection16(
    new ParameterColumn[P1](0),
    new ParameterColumn[P2](1),
    new ParameterColumn[P3](2),
    new ParameterColumn[P4](3),
    new ParameterColumn[P5](4),
    new ParameterColumn[P6](5),
    new ParameterColumn[P7](6),
    new ParameterColumn[P8](7),
    new ParameterColumn[P9](8),
    new ParameterColumn[P10](9),
    new ParameterColumn[P11](10),
    new ParameterColumn[P12](11),
    new ParameterColumn[P13](12),
    new ParameterColumn[P14](13),
    new ParameterColumn[P15](14),
    new ParameterColumn[P16](15)
  ))

  def apply[P1 : TypeMapper, P2 : TypeMapper, P3 : TypeMapper, P4 : TypeMapper, P5 : TypeMapper, P6 : TypeMapper, P7 : TypeMapper, P8 : TypeMapper, P9 : TypeMapper, P10 : TypeMapper, P11 : TypeMapper, P12 : TypeMapper, P13 : TypeMapper, P14 : TypeMapper, P15 : TypeMapper, P16 : TypeMapper, P17 : TypeMapper] =
    new Parameters[(P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14,P15,P16,P17), Projection17[P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14,P15,P16,P17]](new Projection17(
    new ParameterColumn[P1](0),
    new ParameterColumn[P2](1),
    new ParameterColumn[P3](2),
    new ParameterColumn[P4](3),
    new ParameterColumn[P5](4),
    new ParameterColumn[P6](5),
    new ParameterColumn[P7](6),
    new ParameterColumn[P8](7),
    new ParameterColumn[P9](8),
    new ParameterColumn[P10](9),
    new ParameterColumn[P11](10),
    new ParameterColumn[P12](11),
    new ParameterColumn[P13](12),
    new ParameterColumn[P14](13),
    new ParameterColumn[P15](14),
    new ParameterColumn[P16](15),
    new ParameterColumn[P17](16)
  ))

  def apply[P1 : TypeMapper, P2 : TypeMapper, P3 : TypeMapper, P4 : TypeMapper, P5 : TypeMapper, P6 : TypeMapper, P7 : TypeMapper, P8 : TypeMapper, P9 : TypeMapper, P10 : TypeMapper, P11 : TypeMapper, P12 : TypeMapper, P13 : TypeMapper, P14 : TypeMapper, P15 : TypeMapper, P16 : TypeMapper, P17 : TypeMapper, P18 : TypeMapper] =
    new Parameters[(P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14,P15,P16,P17,P18), Projection18[P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14,P15,P16,P17,P18]](new Projection18(
    new ParameterColumn[P1](0),
    new ParameterColumn[P2](1),
    new ParameterColumn[P3](2),
    new ParameterColumn[P4](3),
    new ParameterColumn[P5](4),
    new ParameterColumn[P6](5),
    new ParameterColumn[P7](6),
    new ParameterColumn[P8](7),
    new ParameterColumn[P9](8),
    new ParameterColumn[P10](9),
    new ParameterColumn[P11](10),
    new ParameterColumn[P12](11),
    new ParameterColumn[P13](12),
    new ParameterColumn[P14](13),
    new ParameterColumn[P15](14),
    new ParameterColumn[P16](15),
    new ParameterColumn[P17](16),
    new ParameterColumn[P18](17)
  ))

  def apply[P1 : TypeMapper, P2 : TypeMapper, P3 : TypeMapper, P4 : TypeMapper, P5 : TypeMapper, P6 : TypeMapper, P7 : TypeMapper, P8 : TypeMapper, P9 : TypeMapper, P10 : TypeMapper, P11 : TypeMapper, P12 : TypeMapper, P13 : TypeMapper, P14 : TypeMapper, P15 : TypeMapper, P16 : TypeMapper, P17 : TypeMapper, P18 : TypeMapper, P19 : TypeMapper] =
    new Parameters[(P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14,P15,P16,P17,P18,P19), Projection19[P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14,P15,P16,P17,P18,P19]](new Projection19(
    new ParameterColumn[P1](0),
    new ParameterColumn[P2](1),
    new ParameterColumn[P3](2),
    new ParameterColumn[P4](3),
    new ParameterColumn[P5](4),
    new ParameterColumn[P6](5),
    new ParameterColumn[P7](6),
    new ParameterColumn[P8](7),
    new ParameterColumn[P9](8),
    new ParameterColumn[P10](9),
    new ParameterColumn[P11](10),
    new ParameterColumn[P12](11),
    new ParameterColumn[P13](12),
    new ParameterColumn[P14](13),
    new ParameterColumn[P15](14),
    new ParameterColumn[P16](15),
    new ParameterColumn[P17](16),
    new ParameterColumn[P18](17),
    new ParameterColumn[P19](18)
  ))

  def apply[P1 : TypeMapper, P2 : TypeMapper, P3 : TypeMapper, P4 : TypeMapper, P5 : TypeMapper, P6 : TypeMapper, P7 : TypeMapper, P8 : TypeMapper, P9 : TypeMapper, P10 : TypeMapper, P11 : TypeMapper, P12 : TypeMapper, P13 : TypeMapper, P14 : TypeMapper, P15 : TypeMapper, P16 : TypeMapper, P17 : TypeMapper, P18 : TypeMapper, P19 : TypeMapper, P20 : TypeMapper] =
    new Parameters[(P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14,P15,P16,P17,P18,P19,P20), Projection20[P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14,P15,P16,P17,P18,P19,P20]](new Projection20(
    new ParameterColumn[P1](0),
    new ParameterColumn[P2](1),
    new ParameterColumn[P3](2),
    new ParameterColumn[P4](3),
    new ParameterColumn[P5](4),
    new ParameterColumn[P6](5),
    new ParameterColumn[P7](6),
    new ParameterColumn[P8](7),
    new ParameterColumn[P9](8),
    new ParameterColumn[P10](9),
    new ParameterColumn[P11](10),
    new ParameterColumn[P12](11),
    new ParameterColumn[P13](12),
    new ParameterColumn[P14](13),
    new ParameterColumn[P15](14),
    new ParameterColumn[P16](15),
    new ParameterColumn[P17](16),
    new ParameterColumn[P18](17),
    new ParameterColumn[P19](18),
    new ParameterColumn[P20](19)
  ))

  def apply[P1 : TypeMapper, P2 : TypeMapper, P3 : TypeMapper, P4 : TypeMapper, P5 : TypeMapper, P6 : TypeMapper, P7 : TypeMapper, P8 : TypeMapper, P9 : TypeMapper, P10 : TypeMapper, P11 : TypeMapper, P12 : TypeMapper, P13 : TypeMapper, P14 : TypeMapper, P15 : TypeMapper, P16 : TypeMapper, P17 : TypeMapper, P18 : TypeMapper, P19 : TypeMapper, P20 : TypeMapper, P21 : TypeMapper] =
    new Parameters[(P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14,P15,P16,P17,P18,P19,P20,P21), Projection21[P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14,P15,P16,P17,P18,P19,P20,P21]](new Projection21(
    new ParameterColumn[P1](0),
    new ParameterColumn[P2](1),
    new ParameterColumn[P3](2),
    new ParameterColumn[P4](3),
    new ParameterColumn[P5](4),
    new ParameterColumn[P6](5),
    new ParameterColumn[P7](6),
    new ParameterColumn[P8](7),
    new ParameterColumn[P9](8),
    new ParameterColumn[P10](9),
    new ParameterColumn[P11](10),
    new ParameterColumn[P12](11),
    new ParameterColumn[P13](12),
    new ParameterColumn[P14](13),
    new ParameterColumn[P15](14),
    new ParameterColumn[P16](15),
    new ParameterColumn[P17](16),
    new ParameterColumn[P18](17),
    new ParameterColumn[P19](18),
    new ParameterColumn[P20](19),
    new ParameterColumn[P21](20)
  ))

  def apply[P1 : TypeMapper, P2 : TypeMapper, P3 : TypeMapper, P4 : TypeMapper, P5 : TypeMapper, P6 : TypeMapper, P7 : TypeMapper, P8 : TypeMapper, P9 : TypeMapper, P10 : TypeMapper, P11 : TypeMapper, P12 : TypeMapper, P13 : TypeMapper, P14 : TypeMapper, P15 : TypeMapper, P16 : TypeMapper, P17 : TypeMapper, P18 : TypeMapper, P19 : TypeMapper, P20 : TypeMapper, P21 : TypeMapper, P22 : TypeMapper] =
    new Parameters[(P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14,P15,P16,P17,P18,P19,P20,P21,P22), Projection22[P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14,P15,P16,P17,P18,P19,P20,P21,P22]](new Projection22(
    new ParameterColumn[P1](0),
    new ParameterColumn[P2](1),
    new ParameterColumn[P3](2),
    new ParameterColumn[P4](3),
    new ParameterColumn[P5](4),
    new ParameterColumn[P6](5),
    new ParameterColumn[P7](6),
    new ParameterColumn[P8](7),
    new ParameterColumn[P9](8),
    new ParameterColumn[P10](9),
    new ParameterColumn[P11](10),
    new ParameterColumn[P12](11),
    new ParameterColumn[P13](12),
    new ParameterColumn[P14](13),
    new ParameterColumn[P15](14),
    new ParameterColumn[P16](15),
    new ParameterColumn[P17](16),
    new ParameterColumn[P18](17),
    new ParameterColumn[P19](18),
    new ParameterColumn[P20](19),
    new ParameterColumn[P21](20),
    new ParameterColumn[P22](21)
  ))
}
