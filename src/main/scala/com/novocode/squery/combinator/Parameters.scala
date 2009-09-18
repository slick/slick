package com.novocode.squery.combinator

import java.sql.PreparedStatement
import com.novocode.squery.SQueryException
import com.novocode.squery.combinator.basic.{BasicProfile, BasicQueryTemplate}

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
  def apply[P1](implicit tm1: TypeMapper[_,P1]) = new Parameters[P1, Column[P1]](new ParameterColumn(-1, tm1))

  def apply[P1,P2](implicit
      tm1: TypeMapper[_,P1],
      tm2: TypeMapper[_,P2]
  ) = new Parameters[(P1,P2), Projection2[P1,P2]](new Projection2(
    new ParameterColumn(0, tm1),
    new ParameterColumn(1, tm2)
  ))

  def apply[P1,P2,P3](implicit
      tm1: TypeMapper[_,P1],
      tm2: TypeMapper[_,P2],
      tm3: TypeMapper[_,P3]
  ) = new Parameters[(P1,P2,P3), Projection3[P1,P2,P3]](new Projection3(
    new ParameterColumn(0, tm1),
    new ParameterColumn(1, tm2),
    new ParameterColumn(2, tm3)
  ))

  def apply[P1,P2,P3,P4](implicit
      tm1: TypeMapper[_,P1],
      tm2: TypeMapper[_,P2],
      tm3: TypeMapper[_,P3],
      tm4: TypeMapper[_,P4]
  ) = new Parameters[(P1,P2,P3,P4), Projection4[P1,P2,P3,P4]](new Projection4(
    new ParameterColumn(0, tm1),
    new ParameterColumn(1, tm2),
    new ParameterColumn(2, tm3),
    new ParameterColumn(3, tm4)
  ))

  def apply[P1,P2,P3,P4,P5](implicit
      tm1: TypeMapper[_,P1],
      tm2: TypeMapper[_,P2],
      tm3: TypeMapper[_,P3],
      tm4: TypeMapper[_,P4],
      tm5: TypeMapper[_,P5]
  ) = new Parameters[(P1,P2,P3,P4,P5), Projection5[P1,P2,P3,P4,P5]](new Projection5(
    new ParameterColumn(0, tm1),
    new ParameterColumn(1, tm2),
    new ParameterColumn(2, tm3),
    new ParameterColumn(3, tm4),
    new ParameterColumn(4, tm5)
  ))

  def apply[P1,P2,P3,P4,P5,P6](implicit
      tm1: TypeMapper[_,P1],
      tm2: TypeMapper[_,P2],
      tm3: TypeMapper[_,P3],
      tm4: TypeMapper[_,P4],
      tm5: TypeMapper[_,P5],
      tm6: TypeMapper[_,P6]
  ) = new Parameters[(P1,P2,P3,P4,P5,P6), Projection6[P1,P2,P3,P4,P5,P6]](new Projection6(
    new ParameterColumn(0, tm1),
    new ParameterColumn(1, tm2),
    new ParameterColumn(2, tm3),
    new ParameterColumn(3, tm4),
    new ParameterColumn(4, tm5),
    new ParameterColumn(5, tm6)
  ))

  def apply[P1,P2,P3,P4,P5,P6,P7](implicit
      tm1: TypeMapper[_,P1],
      tm2: TypeMapper[_,P2],
      tm3: TypeMapper[_,P3],
      tm4: TypeMapper[_,P4],
      tm5: TypeMapper[_,P5],
      tm6: TypeMapper[_,P6],
      tm7: TypeMapper[_,P7]
  ) = new Parameters[(P1,P2,P3,P4,P5,P6,P7), Projection7[P1,P2,P3,P4,P5,P6,P7]](new Projection7(
    new ParameterColumn(0, tm1),
    new ParameterColumn(1, tm2),
    new ParameterColumn(2, tm3),
    new ParameterColumn(3, tm4),
    new ParameterColumn(4, tm5),
    new ParameterColumn(5, tm6),
    new ParameterColumn(6, tm7)
  ))

  def apply[P1,P2,P3,P4,P5,P6,P7,P8](implicit
      tm1: TypeMapper[_,P1],
      tm2: TypeMapper[_,P2],
      tm3: TypeMapper[_,P3],
      tm4: TypeMapper[_,P4],
      tm5: TypeMapper[_,P5],
      tm6: TypeMapper[_,P6],
      tm7: TypeMapper[_,P7],
      tm8: TypeMapper[_,P8]
  ) = new Parameters[(P1,P2,P3,P4,P5,P6,P7,P8), Projection8[P1,P2,P3,P4,P5,P6,P7,P8]](new Projection8(
    new ParameterColumn(0, tm1),
    new ParameterColumn(1, tm2),
    new ParameterColumn(2, tm3),
    new ParameterColumn(3, tm4),
    new ParameterColumn(4, tm5),
    new ParameterColumn(5, tm6),
    new ParameterColumn(6, tm7),
    new ParameterColumn(7, tm8)
  ))

  def apply[P1,P2,P3,P4,P5,P6,P7,P8,P9](implicit
      tm1: TypeMapper[_,P1],
      tm2: TypeMapper[_,P2],
      tm3: TypeMapper[_,P3],
      tm4: TypeMapper[_,P4],
      tm5: TypeMapper[_,P5],
      tm6: TypeMapper[_,P6],
      tm7: TypeMapper[_,P7],
      tm8: TypeMapper[_,P8],
      tm9: TypeMapper[_,P9]
  ) = new Parameters[(P1,P2,P3,P4,P5,P6,P7,P8,P9), Projection9[P1,P2,P3,P4,P5,P6,P7,P8,P9]](new Projection9(
    new ParameterColumn(0, tm1),
    new ParameterColumn(1, tm2),
    new ParameterColumn(2, tm3),
    new ParameterColumn(3, tm4),
    new ParameterColumn(4, tm5),
    new ParameterColumn(5, tm6),
    new ParameterColumn(6, tm7),
    new ParameterColumn(7, tm8),
    new ParameterColumn(8, tm9)
  ))

  def apply[P1,P2,P3,P4,P5,P6,P7,P8,P9,P10](implicit
      tm1: TypeMapper[_,P1],
      tm2: TypeMapper[_,P2],
      tm3: TypeMapper[_,P3],
      tm4: TypeMapper[_,P4],
      tm5: TypeMapper[_,P5],
      tm6: TypeMapper[_,P6],
      tm7: TypeMapper[_,P7],
      tm8: TypeMapper[_,P8],
      tm9: TypeMapper[_,P9],
      tm10: TypeMapper[_,P10]
  ) = new Parameters[(P1,P2,P3,P4,P5,P6,P7,P8,P9,P10), Projection10[P1,P2,P3,P4,P5,P6,P7,P8,P9,P10]](new Projection10(
    new ParameterColumn(0, tm1),
    new ParameterColumn(1, tm2),
    new ParameterColumn(2, tm3),
    new ParameterColumn(3, tm4),
    new ParameterColumn(4, tm5),
    new ParameterColumn(5, tm6),
    new ParameterColumn(6, tm7),
    new ParameterColumn(7, tm8),
    new ParameterColumn(8, tm9),
    new ParameterColumn(9, tm10)
  ))

  def apply[P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11](implicit
      tm1: TypeMapper[_,P1],
      tm2: TypeMapper[_,P2],
      tm3: TypeMapper[_,P3],
      tm4: TypeMapper[_,P4],
      tm5: TypeMapper[_,P5],
      tm6: TypeMapper[_,P6],
      tm7: TypeMapper[_,P7],
      tm8: TypeMapper[_,P8],
      tm9: TypeMapper[_,P9],
      tm10: TypeMapper[_,P10],
      tm11: TypeMapper[_,P11]
  ) = new Parameters[(P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11), Projection11[P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11]](new Projection11(
    new ParameterColumn(0, tm1),
    new ParameterColumn(1, tm2),
    new ParameterColumn(2, tm3),
    new ParameterColumn(3, tm4),
    new ParameterColumn(4, tm5),
    new ParameterColumn(5, tm6),
    new ParameterColumn(6, tm7),
    new ParameterColumn(7, tm8),
    new ParameterColumn(8, tm9),
    new ParameterColumn(9, tm10),
    new ParameterColumn(10, tm11)
  ))

  def apply[P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12](implicit
      tm1: TypeMapper[_,P1],
      tm2: TypeMapper[_,P2],
      tm3: TypeMapper[_,P3],
      tm4: TypeMapper[_,P4],
      tm5: TypeMapper[_,P5],
      tm6: TypeMapper[_,P6],
      tm7: TypeMapper[_,P7],
      tm8: TypeMapper[_,P8],
      tm9: TypeMapper[_,P9],
      tm10: TypeMapper[_,P10],
      tm11: TypeMapper[_,P11],
      tm12: TypeMapper[_,P12]
  ) = new Parameters[(P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12), Projection12[P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12]](new Projection12(
    new ParameterColumn(0, tm1),
    new ParameterColumn(1, tm2),
    new ParameterColumn(2, tm3),
    new ParameterColumn(3, tm4),
    new ParameterColumn(4, tm5),
    new ParameterColumn(5, tm6),
    new ParameterColumn(6, tm7),
    new ParameterColumn(7, tm8),
    new ParameterColumn(8, tm9),
    new ParameterColumn(9, tm10),
    new ParameterColumn(10, tm11),
    new ParameterColumn(11, tm12)
  ))

  def apply[P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13](implicit
      tm1: TypeMapper[_,P1],
      tm2: TypeMapper[_,P2],
      tm3: TypeMapper[_,P3],
      tm4: TypeMapper[_,P4],
      tm5: TypeMapper[_,P5],
      tm6: TypeMapper[_,P6],
      tm7: TypeMapper[_,P7],
      tm8: TypeMapper[_,P8],
      tm9: TypeMapper[_,P9],
      tm10: TypeMapper[_,P10],
      tm11: TypeMapper[_,P11],
      tm12: TypeMapper[_,P12],
      tm13: TypeMapper[_,P13]
  ) = new Parameters[(P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13), Projection13[P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13]](new Projection13(
    new ParameterColumn(0, tm1),
    new ParameterColumn(1, tm2),
    new ParameterColumn(2, tm3),
    new ParameterColumn(3, tm4),
    new ParameterColumn(4, tm5),
    new ParameterColumn(5, tm6),
    new ParameterColumn(6, tm7),
    new ParameterColumn(7, tm8),
    new ParameterColumn(8, tm9),
    new ParameterColumn(9, tm10),
    new ParameterColumn(10, tm11),
    new ParameterColumn(11, tm12),
    new ParameterColumn(12, tm13)
  ))

  def apply[P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14](implicit
      tm1: TypeMapper[_,P1],
      tm2: TypeMapper[_,P2],
      tm3: TypeMapper[_,P3],
      tm4: TypeMapper[_,P4],
      tm5: TypeMapper[_,P5],
      tm6: TypeMapper[_,P6],
      tm7: TypeMapper[_,P7],
      tm8: TypeMapper[_,P8],
      tm9: TypeMapper[_,P9],
      tm10: TypeMapper[_,P10],
      tm11: TypeMapper[_,P11],
      tm12: TypeMapper[_,P12],
      tm13: TypeMapper[_,P13],
      tm14: TypeMapper[_,P14]
  ) = new Parameters[(P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14), Projection14[P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14]](new Projection14(
    new ParameterColumn(0, tm1),
    new ParameterColumn(1, tm2),
    new ParameterColumn(2, tm3),
    new ParameterColumn(3, tm4),
    new ParameterColumn(4, tm5),
    new ParameterColumn(5, tm6),
    new ParameterColumn(6, tm7),
    new ParameterColumn(7, tm8),
    new ParameterColumn(8, tm9),
    new ParameterColumn(9, tm10),
    new ParameterColumn(10, tm11),
    new ParameterColumn(11, tm12),
    new ParameterColumn(12, tm13),
    new ParameterColumn(13, tm14)
  ))

  def apply[P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14,P15](implicit
      tm1: TypeMapper[_,P1],
      tm2: TypeMapper[_,P2],
      tm3: TypeMapper[_,P3],
      tm4: TypeMapper[_,P4],
      tm5: TypeMapper[_,P5],
      tm6: TypeMapper[_,P6],
      tm7: TypeMapper[_,P7],
      tm8: TypeMapper[_,P8],
      tm9: TypeMapper[_,P9],
      tm10: TypeMapper[_,P10],
      tm11: TypeMapper[_,P11],
      tm12: TypeMapper[_,P12],
      tm13: TypeMapper[_,P13],
      tm14: TypeMapper[_,P14],
      tm15: TypeMapper[_,P15]
  ) = new Parameters[(P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14,P15), Projection15[P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14,P15]](new Projection15(
    new ParameterColumn(0, tm1),
    new ParameterColumn(1, tm2),
    new ParameterColumn(2, tm3),
    new ParameterColumn(3, tm4),
    new ParameterColumn(4, tm5),
    new ParameterColumn(5, tm6),
    new ParameterColumn(6, tm7),
    new ParameterColumn(7, tm8),
    new ParameterColumn(8, tm9),
    new ParameterColumn(9, tm10),
    new ParameterColumn(10, tm11),
    new ParameterColumn(11, tm12),
    new ParameterColumn(12, tm13),
    new ParameterColumn(13, tm14),
    new ParameterColumn(14, tm15)
  ))

  def apply[P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14,P15,P16](implicit
      tm1: TypeMapper[_,P1],
      tm2: TypeMapper[_,P2],
      tm3: TypeMapper[_,P3],
      tm4: TypeMapper[_,P4],
      tm5: TypeMapper[_,P5],
      tm6: TypeMapper[_,P6],
      tm7: TypeMapper[_,P7],
      tm8: TypeMapper[_,P8],
      tm9: TypeMapper[_,P9],
      tm10: TypeMapper[_,P10],
      tm11: TypeMapper[_,P11],
      tm12: TypeMapper[_,P12],
      tm13: TypeMapper[_,P13],
      tm14: TypeMapper[_,P14],
      tm15: TypeMapper[_,P15],
      tm16: TypeMapper[_,P16]
  ) = new Parameters[(P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14,P15,P16), Projection16[P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14,P15,P16]](new Projection16(
    new ParameterColumn(0, tm1),
    new ParameterColumn(1, tm2),
    new ParameterColumn(2, tm3),
    new ParameterColumn(3, tm4),
    new ParameterColumn(4, tm5),
    new ParameterColumn(5, tm6),
    new ParameterColumn(6, tm7),
    new ParameterColumn(7, tm8),
    new ParameterColumn(8, tm9),
    new ParameterColumn(9, tm10),
    new ParameterColumn(10, tm11),
    new ParameterColumn(11, tm12),
    new ParameterColumn(12, tm13),
    new ParameterColumn(13, tm14),
    new ParameterColumn(14, tm15),
    new ParameterColumn(15, tm16)
  ))

  def apply[P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14,P15,P16,P17](implicit
      tm1: TypeMapper[_,P1],
      tm2: TypeMapper[_,P2],
      tm3: TypeMapper[_,P3],
      tm4: TypeMapper[_,P4],
      tm5: TypeMapper[_,P5],
      tm6: TypeMapper[_,P6],
      tm7: TypeMapper[_,P7],
      tm8: TypeMapper[_,P8],
      tm9: TypeMapper[_,P9],
      tm10: TypeMapper[_,P10],
      tm11: TypeMapper[_,P11],
      tm12: TypeMapper[_,P12],
      tm13: TypeMapper[_,P13],
      tm14: TypeMapper[_,P14],
      tm15: TypeMapper[_,P15],
      tm16: TypeMapper[_,P16],
      tm17: TypeMapper[_,P17]
  ) = new Parameters[(P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14,P15,P16,P17), Projection17[P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14,P15,P16,P17]](new Projection17(
    new ParameterColumn(0, tm1),
    new ParameterColumn(1, tm2),
    new ParameterColumn(2, tm3),
    new ParameterColumn(3, tm4),
    new ParameterColumn(4, tm5),
    new ParameterColumn(5, tm6),
    new ParameterColumn(6, tm7),
    new ParameterColumn(7, tm8),
    new ParameterColumn(8, tm9),
    new ParameterColumn(9, tm10),
    new ParameterColumn(10, tm11),
    new ParameterColumn(11, tm12),
    new ParameterColumn(12, tm13),
    new ParameterColumn(13, tm14),
    new ParameterColumn(14, tm15),
    new ParameterColumn(15, tm16),
    new ParameterColumn(16, tm17)
  ))

  def apply[P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14,P15,P16,P17,P18](implicit
      tm1: TypeMapper[_,P1],
      tm2: TypeMapper[_,P2],
      tm3: TypeMapper[_,P3],
      tm4: TypeMapper[_,P4],
      tm5: TypeMapper[_,P5],
      tm6: TypeMapper[_,P6],
      tm7: TypeMapper[_,P7],
      tm8: TypeMapper[_,P8],
      tm9: TypeMapper[_,P9],
      tm10: TypeMapper[_,P10],
      tm11: TypeMapper[_,P11],
      tm12: TypeMapper[_,P12],
      tm13: TypeMapper[_,P13],
      tm14: TypeMapper[_,P14],
      tm15: TypeMapper[_,P15],
      tm16: TypeMapper[_,P16],
      tm17: TypeMapper[_,P17],
      tm18: TypeMapper[_,P18]
  ) = new Parameters[(P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14,P15,P16,P17,P18), Projection18[P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14,P15,P16,P17,P18]](new Projection18(
    new ParameterColumn(0, tm1),
    new ParameterColumn(1, tm2),
    new ParameterColumn(2, tm3),
    new ParameterColumn(3, tm4),
    new ParameterColumn(4, tm5),
    new ParameterColumn(5, tm6),
    new ParameterColumn(6, tm7),
    new ParameterColumn(7, tm8),
    new ParameterColumn(8, tm9),
    new ParameterColumn(9, tm10),
    new ParameterColumn(10, tm11),
    new ParameterColumn(11, tm12),
    new ParameterColumn(12, tm13),
    new ParameterColumn(13, tm14),
    new ParameterColumn(14, tm15),
    new ParameterColumn(15, tm16),
    new ParameterColumn(16, tm17),
    new ParameterColumn(17, tm18)
  ))

  def apply[P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14,P15,P16,P17,P18,P19](implicit
      tm1: TypeMapper[_,P1],
      tm2: TypeMapper[_,P2],
      tm3: TypeMapper[_,P3],
      tm4: TypeMapper[_,P4],
      tm5: TypeMapper[_,P5],
      tm6: TypeMapper[_,P6],
      tm7: TypeMapper[_,P7],
      tm8: TypeMapper[_,P8],
      tm9: TypeMapper[_,P9],
      tm10: TypeMapper[_,P10],
      tm11: TypeMapper[_,P11],
      tm12: TypeMapper[_,P12],
      tm13: TypeMapper[_,P13],
      tm14: TypeMapper[_,P14],
      tm15: TypeMapper[_,P15],
      tm16: TypeMapper[_,P16],
      tm17: TypeMapper[_,P17],
      tm18: TypeMapper[_,P18],
      tm19: TypeMapper[_,P19]
  ) = new Parameters[(P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14,P15,P16,P17,P18,P19), Projection19[P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14,P15,P16,P17,P18,P19]](new Projection19(
    new ParameterColumn(0, tm1),
    new ParameterColumn(1, tm2),
    new ParameterColumn(2, tm3),
    new ParameterColumn(3, tm4),
    new ParameterColumn(4, tm5),
    new ParameterColumn(5, tm6),
    new ParameterColumn(6, tm7),
    new ParameterColumn(7, tm8),
    new ParameterColumn(8, tm9),
    new ParameterColumn(9, tm10),
    new ParameterColumn(10, tm11),
    new ParameterColumn(11, tm12),
    new ParameterColumn(12, tm13),
    new ParameterColumn(13, tm14),
    new ParameterColumn(14, tm15),
    new ParameterColumn(15, tm16),
    new ParameterColumn(16, tm17),
    new ParameterColumn(17, tm18),
    new ParameterColumn(18, tm19)
  ))

  def apply[P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14,P15,P16,P17,P18,P19,P20](implicit
      tm1: TypeMapper[_,P1],
      tm2: TypeMapper[_,P2],
      tm3: TypeMapper[_,P3],
      tm4: TypeMapper[_,P4],
      tm5: TypeMapper[_,P5],
      tm6: TypeMapper[_,P6],
      tm7: TypeMapper[_,P7],
      tm8: TypeMapper[_,P8],
      tm9: TypeMapper[_,P9],
      tm10: TypeMapper[_,P10],
      tm11: TypeMapper[_,P11],
      tm12: TypeMapper[_,P12],
      tm13: TypeMapper[_,P13],
      tm14: TypeMapper[_,P14],
      tm15: TypeMapper[_,P15],
      tm16: TypeMapper[_,P16],
      tm17: TypeMapper[_,P17],
      tm18: TypeMapper[_,P18],
      tm19: TypeMapper[_,P19],
      tm20: TypeMapper[_,P20]
  ) = new Parameters[(P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14,P15,P16,P17,P18,P19,P20), Projection20[P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14,P15,P16,P17,P18,P19,P20]](new Projection20(
    new ParameterColumn(0, tm1),
    new ParameterColumn(1, tm2),
    new ParameterColumn(2, tm3),
    new ParameterColumn(3, tm4),
    new ParameterColumn(4, tm5),
    new ParameterColumn(5, tm6),
    new ParameterColumn(6, tm7),
    new ParameterColumn(7, tm8),
    new ParameterColumn(8, tm9),
    new ParameterColumn(9, tm10),
    new ParameterColumn(10, tm11),
    new ParameterColumn(11, tm12),
    new ParameterColumn(12, tm13),
    new ParameterColumn(13, tm14),
    new ParameterColumn(14, tm15),
    new ParameterColumn(15, tm16),
    new ParameterColumn(16, tm17),
    new ParameterColumn(17, tm18),
    new ParameterColumn(18, tm19),
    new ParameterColumn(19, tm20)
  ))

  def apply[P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14,P15,P16,P17,P18,P19,P20,P21](implicit
      tm1: TypeMapper[_,P1],
      tm2: TypeMapper[_,P2],
      tm3: TypeMapper[_,P3],
      tm4: TypeMapper[_,P4],
      tm5: TypeMapper[_,P5],
      tm6: TypeMapper[_,P6],
      tm7: TypeMapper[_,P7],
      tm8: TypeMapper[_,P8],
      tm9: TypeMapper[_,P9],
      tm10: TypeMapper[_,P10],
      tm11: TypeMapper[_,P11],
      tm12: TypeMapper[_,P12],
      tm13: TypeMapper[_,P13],
      tm14: TypeMapper[_,P14],
      tm15: TypeMapper[_,P15],
      tm16: TypeMapper[_,P16],
      tm17: TypeMapper[_,P17],
      tm18: TypeMapper[_,P18],
      tm19: TypeMapper[_,P19],
      tm20: TypeMapper[_,P20],
      tm21: TypeMapper[_,P21]
  ) = new Parameters[(P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14,P15,P16,P17,P18,P19,P20,P21), Projection21[P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14,P15,P16,P17,P18,P19,P20,P21]](new Projection21(
    new ParameterColumn(0, tm1),
    new ParameterColumn(1, tm2),
    new ParameterColumn(2, tm3),
    new ParameterColumn(3, tm4),
    new ParameterColumn(4, tm5),
    new ParameterColumn(5, tm6),
    new ParameterColumn(6, tm7),
    new ParameterColumn(7, tm8),
    new ParameterColumn(8, tm9),
    new ParameterColumn(9, tm10),
    new ParameterColumn(10, tm11),
    new ParameterColumn(11, tm12),
    new ParameterColumn(12, tm13),
    new ParameterColumn(13, tm14),
    new ParameterColumn(14, tm15),
    new ParameterColumn(15, tm16),
    new ParameterColumn(16, tm17),
    new ParameterColumn(17, tm18),
    new ParameterColumn(18, tm19),
    new ParameterColumn(19, tm20),
    new ParameterColumn(20, tm21)
  ))

  def apply[P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14,P15,P16,P17,P18,P19,P20,P21,P22](implicit
      tm1: TypeMapper[_,P1],
      tm2: TypeMapper[_,P2],
      tm3: TypeMapper[_,P3],
      tm4: TypeMapper[_,P4],
      tm5: TypeMapper[_,P5],
      tm6: TypeMapper[_,P6],
      tm7: TypeMapper[_,P7],
      tm8: TypeMapper[_,P8],
      tm9: TypeMapper[_,P9],
      tm10: TypeMapper[_,P10],
      tm11: TypeMapper[_,P11],
      tm12: TypeMapper[_,P12],
      tm13: TypeMapper[_,P13],
      tm14: TypeMapper[_,P14],
      tm15: TypeMapper[_,P15],
      tm16: TypeMapper[_,P16],
      tm17: TypeMapper[_,P17],
      tm18: TypeMapper[_,P18],
      tm19: TypeMapper[_,P19],
      tm20: TypeMapper[_,P20],
      tm21: TypeMapper[_,P21],
      tm22: TypeMapper[_,P22]
  ) = new Parameters[(P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14,P15,P16,P17,P18,P19,P20,P21,P22), Projection22[P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14,P15,P16,P17,P18,P19,P20,P21,P22]](new Projection22(
    new ParameterColumn(0, tm1),
    new ParameterColumn(1, tm2),
    new ParameterColumn(2, tm3),
    new ParameterColumn(3, tm4),
    new ParameterColumn(4, tm5),
    new ParameterColumn(5, tm6),
    new ParameterColumn(6, tm7),
    new ParameterColumn(7, tm8),
    new ParameterColumn(8, tm9),
    new ParameterColumn(9, tm10),
    new ParameterColumn(10, tm11),
    new ParameterColumn(11, tm12),
    new ParameterColumn(12, tm13),
    new ParameterColumn(13, tm14),
    new ParameterColumn(14, tm15),
    new ParameterColumn(15, tm16),
    new ParameterColumn(16, tm17),
    new ParameterColumn(17, tm18),
    new ParameterColumn(18, tm19),
    new ParameterColumn(19, tm20),
    new ParameterColumn(20, tm21),
    new ParameterColumn(21, tm22)
  ))
}
