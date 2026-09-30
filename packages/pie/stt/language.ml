let is_letter = function 'a' .. 'z' -> true | _ -> false

let iso639_1 name =
  let lower = String.lowercase_ascii (String.trim name) in
  if String.length lower = 2 && is_letter lower.[0] && is_letter lower.[1] then
    Some lower
  else
    match lower with
    | "polish" -> Some "pl"
    | "english" -> Some "en"
    | "german" -> Some "de"
    | "french" -> Some "fr"
    | "spanish" -> Some "es"
    | "italian" -> Some "it"
    | "portuguese" -> Some "pt"
    | "ukrainian" -> Some "uk"
    | "russian" -> Some "ru"
    | "czech" -> Some "cs"
    | "dutch" -> Some "nl"
    | "japanese" -> Some "ja"
    | "chinese" -> Some "zh"
    | _ -> None
