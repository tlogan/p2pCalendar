structure Util = struct

  val requireLock = "requireLock"
  val lockGranted = "lockGranted"
  val lockDenied = "lockDenied"

  val checkAvailable = "checkAvailable" 
  val notAvailable = "notAvailable" 
  val available = "available" 

  val scheduleMeeting = "scheduleMeeting"
  val scheduled = "scheduled"
  val notScheduled = "notScheduled"

  val timeslot = "timeslot"
  val users = "users"

  val releaseLock = "releaseLock"
  val lockReleased = "lockReleased"
  val notReleased = "notReleased"


  fun recvVecAsync (sock, size) = (let
   val msgIVar = SyncVar.iVar ()
   val _ = CML.spawn (fn () => SyncVar.iPut (msgIVar, Socket.recvVec (sock, size)))
  in
    msgIVar
  end)

  fun sendString (sock, data) = (let
    val dataByteVec = Byte.stringToBytes data
    val dataSlice = Word8VectorSlice.slice (dataByteVec, 0, NONE)
    val _ = Socket.sendVec (sock, dataSlice)
  in
    ()
  end)


  fun sendJson (sock, data) = (let
    val _ = sendString (sock, Json.toString data)
  in
    ()
  end)

end
