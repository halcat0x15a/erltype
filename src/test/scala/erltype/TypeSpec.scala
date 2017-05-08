package erltype

import org.scalatest.FunSuite

class TypeSpec extends FunSuite {
  test("A | A = A") {
    val A = IntType()
    assert(A \/ A == A)
  }
  test("A & A = A") {
    val A = IntType()
    assert(A /\ A == A)
  }
  test("A + B | A + C = A + B + C") {
    val `A + B` = UnionType[Pos](Vector(IntType(), FloatType()))
    val `A + C` = UnionType[Pos](Vector(IntType(), StringType()))
    val `A + B + C` = UnionType[Pos](Vector(IntType(), FloatType(), StringType()))
    assert(`A + B` \/ `A + C` == `A + B + C`)
  }
  test("A + [B] | A + [C] = A + [B + C]") {
    val `A + [B]` = UnionType[Pos](Vector(IntType(), ListType(FloatType())))
    val `A + [C]` = UnionType[Pos](Vector(IntType(), ListType(StringType())))
    val `A + [B + C]` = UnionType[Pos](Vector(IntType(), ListType(UnionType(Vector(FloatType(), StringType())))))
    assert(`A + [B]` \/ `A + [C]` == `A + [B + C]`)
  }
  test("A * B & A * C = A * B * C") {
    val `A * B` = IntersectionType[Neg](Vector(IntType(), FloatType()))
    val `A * C` = IntersectionType[Neg](Vector(IntType(), StringType()))
    val `A * B * C` = IntersectionType[Neg](Vector(IntType(), FloatType(), StringType()))
    assert(`A * B` /\ `A * C` == `A * B * C`)
  }
  test("A * [B] & A * [C] = A * [B * C]") {
    val `A * [B]` = IntersectionType[Neg](Vector(IntType(), ListType(FloatType())))
    val `A * [C]` = IntersectionType[Neg](Vector(IntType(), ListType(StringType())))
    val `A * [B * C]` = IntersectionType[Neg](Vector(IntType(), ListType(IntersectionType(Vector(FloatType(), StringType())))))
    assert(`A * [B]` /\ `A * [C]` == `A * [B * C]`)
  }
  test("A + B & C = A * C + B * C") {
    val `A + B` = UnionType[Neg](Vector(IntType(), FloatType()))
    val C = StringType[Neg]()
    val `A * C + B * C` = UnionType[Neg](Vector(
      IntersectionType(Vector(IntType(), StringType())),
      IntersectionType(Vector(FloatType(), StringType()))
    ))
    assert(`A + B` /\ C == `A * C + B * C`)
  }
  test("A + [B] & [C] = A * [C] + [B * C]") {
    val `A + [B]` = UnionType[Neg](Vector(IntType(), ListType(FloatType())))
    val `[C]` = ListType(StringType[Neg]())
    val `A * [C] + [B * C]` = UnionType[Neg](Vector(
      IntersectionType(Vector(IntType(), ListType(StringType()))),
      ListType(IntersectionType(Vector(FloatType(), StringType())))
    ))
    assert(`A + [B]` /\ `[C]` == `A * [C] + [B * C]`)
  }
  test("A * B | A = A") {
    val `A * B` = IntersectionType[Neg](Vector(IntType(), FloatType()))
    val A = IntType[Neg]()
    assert(`A * B` \/ A == A)
  }
  test("A + B & C + D = A * C + B * C + A * D + B * D") {
    val `A + B` = UnionType[Neg](Vector(IntType(), FloatType()))
    val `C + D` = UnionType[Neg](Vector(CharType(), StringType()))
    val `A * C + B * C + A * D + B * D` = UnionType[Neg](Vector(
      IntersectionType(Vector(IntType(), CharType())),
      IntersectionType(Vector(FloatType(), CharType())),
      IntersectionType(Vector(IntType(), StringType())),
      IntersectionType(Vector(FloatType(), StringType()))
    ))
    assert(`A + B` /\ `C + D` == `A * C + B * C + A * D + B * D`)
  }
  test("A + B & A + C = A + B * C") {
    val `A + B` = UnionType[Neg](Vector(IntType(), FloatType()))
    val `A + C` = UnionType[Neg](Vector(IntType(), StringType()))
    val `A + B * C` = UnionType[Neg](Vector(
      IntType(),
      IntersectionType(Vector(FloatType(), StringType()))
    ))
    assert(`A + B` /\ `A + C` == `A + B * C`)
  }
}
