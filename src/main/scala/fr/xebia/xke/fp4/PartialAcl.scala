package fr.xebia.xke.fp4

object PartialAcl {

  private lazy val adminPassThrough: Restriction = {
    case AdminUser => true
  }

  def lift[Resource](acl: PartialAcl[Resource]): Acl[Resource] = {
    val completeAcl : Resource => User => Boolean = (resource:Resource) => {
      val restrictionAdmin = acl(resource).orElse(adminPassThrough)
      restrictionAdmin.lift.andThen(_.getOrElse(false))
    }
    flip(completeAcl)
  }

  private def flip[A, B, C](f: A => B => C): (B => A => C) = a => b => f(b)(a)

}