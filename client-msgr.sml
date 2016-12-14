structure ClientMsgr = struct

  datatype 'mode t = Msgr of (User.info * 'mode INetSock.stream_sock) list 

  fun requireLocks (Msgr connList) user = (let
    fun ipGt ((si1, _), (si2, _)) = (let
      fun toString (User.Info {username,addr,port}) =  username ^ addr ^ (Int.toString port)
    in
      String.compare(toString si1, toString si2) = GREATER
    end)

    val sortedConnList = ListMergeSort.sort ipGt connList

    fun requireLock (si, sock) = (let
      val data = Json.O [  
        (Util.requireLock, Json.S (User.toString user))
      ]
      val _ = Util.sendJson (sock, data)
      val res = Byte.bytesToString (Socket.recvVec (sock, 1024))
    in
      res = Util.lockGranted 
    end)

    fun lockEach (cs) = (let
    in
      case cs 
        of c::cs' => (let
            val locked = requireLock c
          in
            if locked
            then lockEach cs'
            else false
          end)
         | _ => true
    end)
    val locked = lockEach connList
  in
    locked
  end)

  fun allAvailable (Msgr connList) slot = (let

    fun checkAvailable slot (si, sock)  = (let
      val data = Json.O [  
        (Util.checkAvailable, Json.S (Calendar.timeslotToString slot))
      ]
      val _ = Util.sendJson (sock, data)
      val vecIVar = (Util.recvVecAsync (sock, 1024))
    in
      vecIVar
    end)

    val vecIVarList: (Word8.word vector SyncVar.ivar) list  = map (checkAvailable slot) connList    
    val allAvail = List.all (fn vecIVar => (
      (Byte.bytesToString (SyncVar.iGet vecIVar)) = Util.available
    )) vecIVarList 
  in
    allAvail
  end)

  fun scheduleMeeting (Msgr connList) (slot, users) = (let
    fun scheduleMeetingForOne (slot, users) (si, sock)  = (let
      val data = Json.O [
        (Util.scheduleMeeting, Json.O [
          (Util.timeslot, Json.S (Calendar.timeslotToString slot)),
          (Util.users, Json.A (map (Json.S o User.toString) users))
        ])
      ]
      val _ = Util.sendJson (sock, data)
      val vecIVar = (Util.recvVecAsync (sock, 1024))
    in
      vecIVar
    end)

    val vecIVarList = map (scheduleMeetingForOne (slot, users)) connList
    val allScheduled = List.all (fn vecIVar => (
      (Byte.bytesToString (SyncVar.iGet vecIVar)) = Util.scheduled
    )) vecIVarList 
  in
    allScheduled
  end)

  fun releaseLocks (Msgr connList) user = (let
    fun compare ((si1, _), (si2, _)) = (let
      fun toString (User.Info {username,addr,port}) =  username ^ addr ^ (Int.toString port)
    in
      String.compare(toString si1, toString si2) = LESS 
    end)

    val sortedConnList = ListMergeSort.sort compare connList

    fun releaseLock (si, sock) = (let
      val data = Json.O [  
        (Util.releaseLock, Json.S (User.toString user))
      ]
      val _ = Util.sendJson (sock, data)
      val res = Byte.bytesToString (Socket.recvVec (sock, 1024))
      val _ = Socket.close sock
    in
      res = Util.lockReleased 
    end)

    fun releaseEach (cs) = (let
    in
      case cs 
        of c::cs' => (let
            val released = releaseLock c
          in
            if released 
            then releaseEach cs'
            else false
          end)
         | _ => true
    end)
    val released = releaseEach connList
  in
    released 
  end)

  fun create (serverList) = (let
    fun connect (si) = (let
      val (User.Info {addr, port, username}) = si
      val (sock, msgIVar) = Client.connectToServer (valOf (NetHostDB.fromString addr), port)
      val msgOp = CML.select [ 
        CML.wrap(CML.timeOutEvt (Time.fromSeconds 3), fn () => NONE),
        CML.wrap(SyncVar.iGetEvt msgIVar, fn msg => SOME (Byte.bytesToString msg))
      ]
    in
      case msgOp
        of SOME msg => (
              if msg = username 
              then SOME (si, sock)
              else (Socket.close sock; NONE)
            )
         | NONE => (Socket.close sock; NONE)
    end)

    val connOps = map connect serverList 
    val connList = (map valOf connOps)
  in
    if (List.exists (not o Option.isSome) connOps)
    then raise (Fail "connection")
    else Msgr connList 
  end)

end
