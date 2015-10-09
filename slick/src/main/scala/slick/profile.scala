package slick

package object profile {
  @deprecated("Use type `slick.basic.BasicProfile` instead of `slick.profile.BasicDriver`", "3.2")
  type BasicDriver = slick.basic.BasicProfile

  @deprecated("Use type `slick.basic.BasicProfile` instead of `slick.profile.BasicProfile`", "3.2")
  type BasicProfile = slick.basic.BasicProfile

  @deprecated("Use `slick.basic.Capability` instead of `slick.profile.Capability`", "3.2")
  type Capability = slick.basic.Capability
  @deprecated("Use `slick.basic.Capability` instead of `slick.profile.Capability`", "3.2")
  val Capability = slick.basic.Capability

  @deprecated("Use type `slick.relational.RelationalProfile` instead of `slick.profile.RelationalDriver`", "3.2")
  type RelationalDriver = slick.relational.RelationalProfile

  @deprecated("Use type `slick.relational.RelationalProfile` instead of `slick.profile.RelationalProfile`", "3.2")
  type RelationalProfile = slick.relational.RelationalProfile
  object RelationalProfile {
    @deprecated("Use object `slick.relational.RelationalCapabilities` instead of `slick.profile.RelationalProfile.capabilities`", "3.2")
    val capabilities = slick.relational.RelationalCapabilities
  }

  @deprecated("Use type `slick.sql.SqlProfile` instead of `slick.profile.SqlDriver`", "3.2")
  type SqlDriver = slick.sql.SqlProfile

  @deprecated("Use type `slick.sql.SqlProfile` instead of `slick.profile.SqlProfile`", "3.2")
  type SqlProfile = slick.sql.SqlProfile
  object SqlProfile {
    @deprecated("Use object `slick.sql.SqlCapabilities` instead of `slick.profile.SqlProfile.capabilities`", "3.2")
    val capabilities = slick.sql.SqlCapabilities
  }
}
