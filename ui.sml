signature UI = sig 

  type ui 

  val run: unit -> ui

  val getUsername: ui -> string
  val getServerPort: ui -> int 
  val setStore: ui * Storage.storage -> unit 

end

structure UI :> UI = struct

  datatype ui = UI of (string SyncVar.ivar) * (int SyncVar.ivar) * (Storage.storage SyncVar.ivar)
  datatype state = Username | ServerPort | Command

  fun getUsername (UI (usernameIVar, _, _)) = SyncVar.iGet usernameIVar 

  fun getServerPort (UI (_, portIVar, _)) = SyncVar.iGet portIVar 

  fun setStore (UI (_, _, storeIVar), store) = SyncVar.iPut (storeIVar, store)

  fun run () = (let 

    val usernameIVar = SyncVar.iVar()
    val serverPortIVar = SyncVar.iVar()
    val storeIVar = SyncVar.iVar()

    val _ = (
      print(CommandLine.name()) ;
      print "\n" ;
      app (fn s => (print s ; print "\n")) (CommandLine.arguments())
    )

    fun validateUsername usernameStr = (let
      val tokens = String.tokens Char.isSpace usernameStr 
    in
      (List.length tokens) = 1
    end)

    fun handleUsername () = (let
      val _ = print "Create a username: ";
      val _ = case TextIO.inputLine TextIO.stdIn 
        of NONE => () 
         | SOME txt => (let
           val cleanUsername = String.substring (txt, 0, (String.size txt) - 1)
         in
           if validateUsername cleanUsername 
           then (SyncVar.iPut (usernameIVar, cleanUsername); print ("Userame: " ^ txt))
           else (print "Username cannot contain blank spaces\n"; handleUsername ())
         end) 
    in
      ()
    end)

    fun handleServerPort () = (let
      val _ = print "Enter port number for server (default: 5555): ";
      val _ = case TextIO.inputLine TextIO.stdIn 
        of NONE => () 
         | SOME txt => 
           if txt = "\n"
           then (SyncVar.iPut (serverPortIVar, 5555); print ("Server Port: 5555\n"))
           else (case (Int.fromString txt)
             of NONE => (print "Port must be a number.\n"; handleServerPort ())
              | SOME num => (SyncVar.iPut (serverPortIVar, num); print ("Server Port: " ^ txt))
           )
    in
      ()
    end)

    fun printUsers serverInfoList = (let
      fun toString (si, shortId) = (
        (Int.toString shortId) ^ ": " ^ (User.toString si) ^ "\n"
      )
      val strList = map toString serverInfoList
    in
      print (String.concat strList)
    end)

    fun listUsers () = (let
      val storeOp = SyncVar.iGetPoll storeIVar
    in
      case storeOp 
        of NONE => print "There are no other users online.\n"
         | SOME store => printUsers (Storage.getServerInfoList store)
    end)

    fun viewCalendar () = (let
      val store = SyncVar.iGet storeIVar
      val calendar = Storage.getCalendar store 
    in
      print ((Calendar.toString calendar) ^ "\n")
    end)

    fun addMeeting (slot, shortIds) = (let
      val store = SyncVar.iGet storeIVar
    in
      Storage.requestMeeting (store, slot, shortIds)
    end)

    datatype command = ListUsers 
                     | ViewCalendar
                     | AddMeeting of Calendar.timeslot * int list
                     | Nada

    fun matchCommand tokens = (let
      val lsize = List.length tokens
      val cleanStr = String.concatWith " " (List.take (tokens, Int.min (lsize, 2))) 
    in
      if cleanStr = "list users" then ListUsers
      else if cleanStr = "view calendar" then ViewCalendar 
      else if cleanStr = "add meeting" andalso (List.length tokens) >= 4 then (let
        val startTime = List.nth (tokens, 2) 
        val endTime = List.nth (tokens, 3) 
        val shortIds = map (valOf o Int.fromString) (List.drop (tokens, 3)) 
      in
        (AddMeeting (Calendar.parseTimeslot (startTime ^ " " ^ endTime), shortIds)) handle _ => Nada
      end)
      else Nada
    end)

    fun handleCommand str = (let
      val tokens = String.tokens Char.isSpace str 
      val commandMatch = matchCommand tokens 
    in
      case commandMatch 
        of Nada => ()
         | ListUsers => listUsers ()
         | ViewCalendar => viewCalendar ()
         | AddMeeting m => addMeeting m 
    end)

    fun handleCommands () = (let
      val _ = print "> "
    in
      case TextIO.inputLine TextIO.stdIn
        of NONE => ()
         | SOME inputStr => (handleCommand inputStr)
    end)

    fun loop state = (case state 
      of Username => (handleUsername (); loop ServerPort)
       | ServerPort => (handleServerPort (); loop Command)
       | Command => (handleCommands (); loop Command)
    )

    val _ = CML.spawn (fn () => loop Username) 
  in 
    UI (usernameIVar, serverPortIVar, storeIVar)
  end)

end

