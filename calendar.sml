signature CALENDAR = sig
  type timeslot
  type calendar

  val timeslot: int * int -> timeslot 
  val calendar: unit -> calendar
  val overlap: calendar * timeslot -> bool
  val fillTimeslot: calendar * timeslot * (User.info list) -> calendar 

  val timeslotToString: timeslot -> string
  val parseTimeslot: string -> timeslot  

  val toString: calendar -> string
end

structure Calendar :> CALENDAR = struct

  type day = int
  type hour = int
  datatype timeslot = Slot of day * hour 
  datatype calendar = Cal of (timeslot * User.info list) list  

  fun timeslot (day, hour) = (let
  in
    if (
      day >= 0 andalso day < 5
      andalso hour >= 0 andalso hour < 24 
    ) then Slot (day, hour)
    else raise Domain
  end)


  fun lte (Slot (d1, h1), Slot (d2, h2)) = (let   
  in
    d1 < d2 
    orelse (d1 = d2 andalso h1 <= h2)
  end)

  fun gte (p1, p2) = lte (p2, p1)

  val gt = not o lte    

  fun lt (p1, p2) = gt (p2, p1)

  fun calendar () = (let 
  in
    Cal []
  end)

  fun overlap (Cal ss, slot) = case ss
    of (s, users)::ss' => (
      s = slot orelse overlap (Cal ss', slot)
    ) 
    | _ => false


  fun timeslotToString (Slot (d, h)) =  (let
    val dayStr = if d = 0 then "mon"
                 else if d = 1 then "tue"
                 else if d = 2 then "wed"
                 else if d = 3 then "thu"
                 else if d = 4 then "fri"
                 else raise Domain

    val hourStr = (Int.toString h)
  in
    String.concatWith ":" [dayStr, hourStr] 
  end)

  fun parseTimeslot str = (let
    val tokens = String.tokens (fn c => c = #":") str 
    val dayStr = List.nth (tokens, 0) 
    val hourStr = List.nth (tokens, 1) 

    val dayNum = if dayStr = "mon" then 0 
                 else if dayStr = "tue" then 1 
                 else if dayStr = "wed" then 2 
                 else if dayStr = "thu" then 3 
                 else if dayStr = "fri" then 4 
                 else raise Domain

    val hourNum = valOf (Int.fromString hourStr)
  in
    timeslot (dayNum, hourNum)
  end)


  fun toString (Cal ss) = (let
    fun pairToString (slot, users) = (
      (timeslotToString slot) ^ " [" ^ (String.concatWith " " (map User.toString users)) ^ "]"
    )
    val strList = map pairToString ss
  in
    if null strList
    then "Nothing in calendar"
    else String.concatWith "\n" strList
  end)

  fun fillTimeslot (Cal ss, slot, users) = (let
    fun compare ((s1, _), (s2, _)) =  gt (s1, s2)
  in
    if not (overlap (Cal ss, slot))
    then Cal (ListMergeSort.sort compare ((slot, users)::ss))
    else (Cal ss) 
  end)


end
