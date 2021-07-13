package zio

trait Unzippable[A, B] {
  type In
  def unzip(in: In): (A, B)
}

object Unzippable extends UnzippableLowPriority1 {

  type In[A, B, C] = Unzippable[A, B] { type In = C }

  implicit def UnzippableLeftIdentity[A]: Unzippable.In[Unit, A, A] =
    new Unzippable[Unit, A] {
      type In = A
      def unzip(in: A): (Unit, A) =
        ((), in)
    }
}

trait UnzippableLowPriority1 extends UnzippableLowPriority2 {

  implicit def UnzippableRightIdentity[A]: Unzippable.In[A, Unit, A] =
    new Unzippable[A, Unit] {
      type In = A
      def unzip(in: A): (A, Unit) =
        (in, ())
    }
}

trait UnzippableLowPriority2 extends UnzippableLowPriority3 {

  implicit def Unzippable3[A, B, Z]: Unzippable.In[(A, B), Z, (A, B, Z)] =
    new Unzippable[(A, B), Z] {
      type In = (A, B, Z)
      def unzip(in: (A, B, Z)): ((A, B), Z) =
        ((in._1, in._2), in._3)
    }

  implicit def Unzippable4[A, B, C, Z]: Unzippable.In[(A, B, C), Z, (A, B, C, Z)] =
    new Unzippable[(A, B, C), Z] {
      type In = (A, B, C, Z)
      def unzip(in: (A, B, C, Z)): ((A, B, C), Z) =
        ((in._1, in._2, in._3), in._4)
    }

  implicit def Unzippable5[A, B, C, D, Z]: Unzippable.In[(A, B, C, D), Z, (A, B, C, D, Z)] =
    new Unzippable[(A, B, C, D), Z] {
      type In = (A, B, C, D, Z)
      def unzip(in: (A, B, C, D, Z)): ((A, B, C, D), Z) =
        ((in._1, in._2, in._3, in._4), in._5)
    }

  implicit def Unzippable6[A, B, C, D, E, Z]: Unzippable.In[(A, B, C, D, E), Z, (A, B, C, D, E, Z)] =
    new Unzippable[(A, B, C, D, E), Z] {
      type In = (A, B, C, D, E, Z)
      def unzip(in: (A, B, C, D, E, Z)): ((A, B, C, D, E), Z) =
        ((in._1, in._2, in._3, in._4, in._5), in._6)
    }

  implicit def Unzippable7[A, B, C, D, E, F, Z]: Unzippable.In[(A, B, C, D, E, F), Z, (A, B, C, D, E, F, Z)] =
    new Unzippable[(A, B, C, D, E, F), Z] {
      type In = (A, B, C, D, E, F, Z)
      def unzip(in: (A, B, C, D, E, F, Z)): ((A, B, C, D, E, F), Z) =
        ((in._1, in._2, in._3, in._4, in._5, in._6), in._7)
    }

  implicit def Unzippable8[A, B, C, D, E, F, G, Z]: Unzippable.In[(A, B, C, D, E, F, G), Z, (A, B, C, D, E, F, G, Z)] =
    new Unzippable[(A, B, C, D, E, F, G), Z] {
      type In = (A, B, C, D, E, F, G, Z)
      def unzip(in: (A, B, C, D, E, F, G, Z)): ((A, B, C, D, E, F, G), Z) =
        ((in._1, in._2, in._3, in._4, in._5, in._6, in._7), in._8)
    }

  implicit def Unzippable9[A, B, C, D, E, F, G, H, Z]
    : Unzippable.In[(A, B, C, D, E, F, G, H), Z, (A, B, C, D, E, F, G, H, Z)] =
    new Unzippable[(A, B, C, D, E, F, G, H), Z] {
      type In = (A, B, C, D, E, F, G, H, Z)
      def unzip(in: (A, B, C, D, E, F, G, H, Z)): ((A, B, C, D, E, F, G, H), Z) =
        ((in._1, in._2, in._3, in._4, in._5, in._6, in._7, in._8), in._9)
    }

  implicit def Unzippable10[A, B, C, D, E, F, G, H, I, Z]
    : Unzippable.In[(A, B, C, D, E, F, G, H, I), Z, (A, B, C, D, E, F, G, H, I, Z)] =
    new Unzippable[(A, B, C, D, E, F, G, H, I), Z] {
      type In = (A, B, C, D, E, F, G, H, I, Z)
      def unzip(in: (A, B, C, D, E, F, G, H, I, Z)): ((A, B, C, D, E, F, G, H, I), Z) =
        ((in._1, in._2, in._3, in._4, in._5, in._6, in._7, in._8, in._9), in._10)
    }

  implicit def Unzippable11[A, B, C, D, E, F, G, H, I, J, Z]
    : Unzippable.In[(A, B, C, D, E, F, G, H, I, J), Z, (A, B, C, D, E, F, G, H, I, J, Z)] =
    new Unzippable[(A, B, C, D, E, F, G, H, I, J), Z] {
      type In = (A, B, C, D, E, F, G, H, I, J, Z)
      def unzip(in: (A, B, C, D, E, F, G, H, I, J, Z)): ((A, B, C, D, E, F, G, H, I, J), Z) =
        ((in._1, in._2, in._3, in._4, in._5, in._6, in._7, in._8, in._9, in._10), in._11)
    }

  implicit def Unzippable12[A, B, C, D, E, F, G, H, I, J, K, Z]
    : Unzippable.In[(A, B, C, D, E, F, G, H, I, J, K), Z, (A, B, C, D, E, F, G, H, I, J, K, Z)] =
    new Unzippable[(A, B, C, D, E, F, G, H, I, J, K), Z] {
      type In = (A, B, C, D, E, F, G, H, I, J, K, Z)
      def unzip(in: (A, B, C, D, E, F, G, H, I, J, K, Z)): ((A, B, C, D, E, F, G, H, I, J, K), Z) =
        ((in._1, in._2, in._3, in._4, in._5, in._6, in._7, in._8, in._9, in._10, in._11), in._12)
    }

  implicit def Unzippable13[A, B, C, D, E, F, G, H, I, J, K, L, Z]
    : Unzippable.In[(A, B, C, D, E, F, G, H, I, J, K, L), Z, (A, B, C, D, E, F, G, H, I, J, K, L, Z)] =
    new Unzippable[(A, B, C, D, E, F, G, H, I, J, K, L), Z] {
      type In = (A, B, C, D, E, F, G, H, I, J, K, L, Z)
      def unzip(in: (A, B, C, D, E, F, G, H, I, J, K, L, Z)): ((A, B, C, D, E, F, G, H, I, J, K, L), Z) =
        ((in._1, in._2, in._3, in._4, in._5, in._6, in._7, in._8, in._9, in._10, in._11, in._12), in._13)
    }

  implicit def Unzippable14[A, B, C, D, E, F, G, H, I, J, K, L, M, Z]
    : Unzippable.In[(A, B, C, D, E, F, G, H, I, J, K, L, M), Z, (A, B, C, D, E, F, G, H, I, J, K, L, M, Z)] =
    new Unzippable[(A, B, C, D, E, F, G, H, I, J, K, L, M), Z] {
      type In = (A, B, C, D, E, F, G, H, I, J, K, L, M, Z)
      def unzip(in: (A, B, C, D, E, F, G, H, I, J, K, L, M, Z)): ((A, B, C, D, E, F, G, H, I, J, K, L, M), Z) =
        ((in._1, in._2, in._3, in._4, in._5, in._6, in._7, in._8, in._9, in._10, in._11, in._12, in._13), in._14)
    }

  implicit def Unzippable15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, Z]
    : Unzippable.In[(A, B, C, D, E, F, G, H, I, J, K, L, M, N), Z, (A, B, C, D, E, F, G, H, I, J, K, L, M, N, Z)] =
    new Unzippable[(A, B, C, D, E, F, G, H, I, J, K, L, M, N), Z] {
      type In = (A, B, C, D, E, F, G, H, I, J, K, L, M, N, Z)
      def unzip(in: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, Z)): ((A, B, C, D, E, F, G, H, I, J, K, L, M, N), Z) =
        (
          (in._1, in._2, in._3, in._4, in._5, in._6, in._7, in._8, in._9, in._10, in._11, in._12, in._13, in._14),
          in._15
        )
    }

  implicit def Unzippable16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, Z]: Unzippable.In[
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O),
    Z,
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, Z)
  ] =
    new Unzippable[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O), Z] {
      type In = (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, Z)
      def unzip(
        in: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, Z)
      ): ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O), Z) =
        (
          (
            in._1,
            in._2,
            in._3,
            in._4,
            in._5,
            in._6,
            in._7,
            in._8,
            in._9,
            in._10,
            in._11,
            in._12,
            in._13,
            in._14,
            in._15
          ),
          in._16
        )
    }

  implicit def Unzippable17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Z]: Unzippable.In[
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P),
    Z,
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Z)
  ] =
    new Unzippable[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P), Z] {
      type In = (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Z)
      def unzip(
        in: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Z)
      ): ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P), Z) =
        (
          (
            in._1,
            in._2,
            in._3,
            in._4,
            in._5,
            in._6,
            in._7,
            in._8,
            in._9,
            in._10,
            in._11,
            in._12,
            in._13,
            in._14,
            in._15,
            in._16
          ),
          in._17
        )
    }

  implicit def Unzippable18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, Z]: Unzippable.In[
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q),
    Z,
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, Z)
  ] =
    new Unzippable[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q), Z] {
      type In = (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, Z)
      def unzip(
        in: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, Z)
      ): ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q), Z) =
        (
          (
            in._1,
            in._2,
            in._3,
            in._4,
            in._5,
            in._6,
            in._7,
            in._8,
            in._9,
            in._10,
            in._11,
            in._12,
            in._13,
            in._14,
            in._15,
            in._16,
            in._17
          ),
          in._18
        )
    }

  implicit def Unzippable19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, Z]: Unzippable.In[
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R),
    Z,
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, Z)
  ] =
    new Unzippable[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R), Z] {
      type In = (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, Z)
      def unzip(
        in: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, Z)
      ): ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R), Z) =
        (
          (
            in._1,
            in._2,
            in._3,
            in._4,
            in._5,
            in._6,
            in._7,
            in._8,
            in._9,
            in._10,
            in._11,
            in._12,
            in._13,
            in._14,
            in._15,
            in._16,
            in._17,
            in._18
          ),
          in._19
        )
    }

  implicit def Unzippable20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, Z]: Unzippable.In[
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S),
    Z,
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, Z)
  ] =
    new Unzippable[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S), Z] {
      type In = (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, Z)
      def unzip(
        in: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, Z)
      ): ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S), Z) =
        (
          (
            in._1,
            in._2,
            in._3,
            in._4,
            in._5,
            in._6,
            in._7,
            in._8,
            in._9,
            in._10,
            in._11,
            in._12,
            in._13,
            in._14,
            in._15,
            in._16,
            in._17,
            in._18,
            in._19
          ),
          in._20
        )
    }

  implicit def Unzippable21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, Z]: Unzippable.In[
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T),
    Z,
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, Z)
  ] =
    new Unzippable[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T), Z] {
      type In = (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, Z)
      def unzip(
        in: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, Z)
      ): ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T), Z) =
        (
          (
            in._1,
            in._2,
            in._3,
            in._4,
            in._5,
            in._6,
            in._7,
            in._8,
            in._9,
            in._10,
            in._11,
            in._12,
            in._13,
            in._14,
            in._15,
            in._16,
            in._17,
            in._18,
            in._19,
            in._20
          ),
          in._21
        )
    }

  implicit def Unzippable22[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, Z]: Unzippable.In[
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U),
    Z,
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, Z)
  ] =
    new Unzippable[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U), Z] {
      type In = (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, Z)
      def unzip(
        in: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, Z)
      ): ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U), Z) =
        (
          (
            in._1,
            in._2,
            in._3,
            in._4,
            in._5,
            in._6,
            in._7,
            in._8,
            in._9,
            in._10,
            in._11,
            in._12,
            in._13,
            in._14,
            in._15,
            in._16,
            in._17,
            in._18,
            in._19,
            in._20,
            in._21
          ),
          in._22
        )
    }
}

trait UnzippableLowPriority3 {

  implicit def Unzippable2[A, B]: Unzippable.In[A, B, (A, B)] =
    new Unzippable[A, B] {
      type In = (A, B)
      def unzip(in: (A, B)): (A, B) =
        (in._1, in._2)
    }
}
