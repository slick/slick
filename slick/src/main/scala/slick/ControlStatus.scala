package slick

final case class ControlStatus(
  availableConnectionSlots: Long,
  pendingConnectionSlots: Int,
  availableAdmissionQueueSlots: Long,
  availableInflightSlots: Long
)
