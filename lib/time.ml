let pluralize singular plural n =
  if n == 1 then "1 " ^ singular else string_of_int n ^ " " ^ plural

let to_mins_string = pluralize "min" "mins"
let to_hours_string = pluralize "hour" "hours"
let to_days_string = pluralize "day" "days"

let seconds_to_string secs =
  let days = secs / 86_400
  and hours = secs / 3_600 mod 24
  and mins = secs / 60 mod 60 in
  ((if days > 0 then [ to_days_string days ] else [])
  @ (if hours > 0 then [ to_hours_string hours ] else [])
  @ if mins > 0 then [ to_mins_string mins ] else [])
  |> String.concat " "
