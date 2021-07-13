package zio.test.internal

trait OptionalImplicit[A] {
  def value: Option[A]
}

object OptionalImplicit extends LowPriOptionalImplicit {
  def apply[A: OptionalImplicit]: Option[A] = implicitly[OptionalImplicit[A]].value

  implicit def some[A](implicit instance: A): OptionalImplicit[A] = new OptionalImplicit[A] {
    val value: Option[A] = Some(instance)
  }
}

trait LowPriOptionalImplicit {
  implicit def none[A]: OptionalImplicit[A] = new OptionalImplicit[A] {
    val value: Option[A] = None
  }
}
