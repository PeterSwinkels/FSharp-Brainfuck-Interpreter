//The namespaces used by this module.
open System
open System.Collections.Generic 
open System.Diagnostics
open System.IO
open System.Reflection

let ProgramInformation = FileVersionInfo.GetVersionInfo(Assembly.GetExecutingAssembly().Location) //Contains this program's information.

//This function converts the specified number to a padded hexadecimal string.
let ToHexadecimal number = 
   (if number < 0x10 then "0" else "") + (number |> sprintf("%X"))

//This function converts non-displayable characters in the specified text to escape sequences.
let Escape (text:string) = 
   let rec ParseCharacters characters escaped =
      if characters = [] then
         escaped
      else
         ParseCharacters characters.Tail (escaped + 
            match characters.Head with
            | character when character = '/' -> (character |> string) + (character |> string)
            | character when character = '\t' || character >= ' ' -> character |> string
            | _ -> "/" + ToHexadecimal(characters.Head |> int))
   ParseCharacters (text.ToCharArray() |> Array.toList) ""

//This function convert the specified escape sequence into a character.
let EscapedCharacter (text:string) = 
   if text.Substring(0, 2) = "//" then
      Some('/')
   else
      try
         Some("0x" + (text.Substring(1, 2)) |> int |> char)
      with
      | _ -> None

//This function returns the specified escape sequence's length.
let EscapedLength (text:string) =    
   if text.Length >= 2 && text.Substring(0, 2) = "//" then
      2
   else if text.Length >= 3 && (not (EscapedCharacter(text) = None))  then
      3
   else
      0

//This function manages the list of loops in the code being executed.
let GetLoops (code:string) = 
   let rec Parse (code:string) instructionP noLoopStart loopEndStack (loopTable:Dictionary<int, int>) = 
      if instructionP >= code.Length || noLoopStart then
         if not (List.isEmpty(loopEndStack)) then
            printfn "Loop without end."
            loopTable
         else
            loopTable
      else if code.Chars(instructionP) = '[' then
         Parse code (instructionP + 0x1) noLoopStart (loopEndStack @ [instructionP]) loopTable 
      else if code.Chars(instructionP) = ']' && List.isEmpty(loopEndStack) then
         printfn "End of loop without start."
         Parse code (instructionP + 0x1) true loopEndStack loopTable
      else if code.Chars(instructionP) = ']' then
         let EndOfLoop = List.last(loopEndStack)
         loopTable.Add(EndOfLoop, instructionP)
         loopTable.Add(instructionP, EndOfLoop)
         Parse code (instructionP + 0x1) noLoopStart (loopEndStack |> List.take(loopEndStack.Length - 1)) loopTable
      else
         Parse code (instructionP + 0x1) noLoopStart loopEndStack loopTable

   Parse code 0 false [] (new Dictionary<int, int>())

//This function checks whether the specified string contains an invalid escape sequence.
let InvalidEscapeAt (text:string) =
   let rec Parse (characters:list<char>) position = 
      if characters = [] then
         0
      else
         if characters.Head = '/' then
            match EscapedLength(String.Concat(characters)) with
            | 2 -> Parse characters.Tail.Tail (position + 2)
            | 3 -> Parse characters.Tail.Tail.Tail (position + 3)
            | _ -> position + 1
         else
            Parse characters.Tail (position + 1)
   Parse (text.ToCharArray() |> Array.toList) 0

//This function unescaped the specified text.
let Unescape (text:string) =
   let rec Parse (characters:list<char>) (unescaped:list<char>) = 
      if List.isEmpty(characters) then
         unescaped |> Seq.toArray |> String
      else
         if characters.Head = '/' then
            let text = String.Concat(characters)

            match EscapedLength(text) with
            | 2 -> Parse characters.Tail.Tail (unescaped @ [EscapedCharacter(text).Value])
            | 3 -> Parse characters.Tail.Tail.Tail (unescaped @ [EscapedCharacter(text).Value])
            | _ -> unescaped |> Seq.toArray |> String
         else
            Parse characters.Tail (unescaped @ [characters.Head])
   Parse (text.ToCharArray() |> Array.toList) []

//This function requests the user to enter input.
let rec GetInput prompt =
   printf prompt
   let Text = Console.ReadLine()
   let ErrorAt = InvalidEscapeAt Text
   if ErrorAt = 0 then      
      Unescape Text
   else
      printfn  "Bad escape sequence at character #%d." ErrorAt
      GetInput prompt

//This function executes the specified code.
let Execute (executeCode:string) (inputLineBreak:string) (outputLineBreak:string) = 
   let NewMemoryP instructionP memoryP (memory:List<Byte>) = 
      match executeCode.Chars(instructionP) with
      | '>' -> if memoryP = memory.Count - 0x1 then 0x0 else memoryP + 0x1
      | '<'-> if memoryP = 0x0 then memory.Count - 0x1 else memoryP - 0x1
      | _ -> memoryP

   let ProcessLoop instructionP memoryP (memory:List<Byte>) (loopTable:Dictionary<int, int>) = 
      match executeCode.Chars(instructionP) with
      | '[' ->
         match memory.[memoryP] with
         | 0x0uy -> loopTable.Item(instructionP)
         | _ -> instructionP
      | ']' ->
         match memory.[memoryP] with
         | 0x0uy -> instructionP
         | _ -> loopTable.Item(instructionP)
      |_ ->
         instructionP

   let UserInput = new Queue<char>()

   let rec ExecutionLoop instructionP memoryP (memory:List<Byte>) (loopTable:Dictionary<int, int>) (outputBuffer:string) = 
      if instructionP >= 0x0 && instructionP < executeCode.Length then
         memory.[memoryP] <-
            match executeCode.Chars(instructionP) with
            | '+' ->
               match memory.[memoryP] with
               | 0xFFuy -> 0x0uy
               | _ -> memory.[memoryP] + 0x1uy
            | '-' ->
               match memory.[memoryP] with
               | 0x0uy -> 0xFFuy
               | _ -> memory.[memoryP] - 0x1uy
            | _ ->
               memory.[memoryP]

         if executeCode.Chars(instructionP) = ',' then
            if UserInput.Count = 0 then
               (GetInput("") + inputLineBreak).ToCharArray() |> Array.toList |> List.map(fun character -> UserInput.Enqueue(character)) |> ignore
            if UserInput.Count > 0 then memory.[memoryP] <- (UserInput.Dequeue() |> byte)

            ExecutionLoop (instructionP + 0x1) memoryP memory loopTable outputBuffer
         else if executeCode.Chars(instructionP) = '.' then
            let Character = (memory.[memoryP] |> char |> string)
            
            if (outputBuffer + Character).EndsWith(outputLineBreak) then
               printf "\n"
               ExecutionLoop (instructionP + 0x1) memoryP memory loopTable ""
            else
               printf "%s" (Escape Character)
               ExecutionLoop (instructionP + 0x1) memoryP memory loopTable (outputBuffer + Character)
         else if executeCode.Chars(instructionP) = '[' || executeCode.Chars(instructionP) = ']' then
            ExecutionLoop ((ProcessLoop instructionP memoryP memory loopTable) + 0x1) memoryP memory loopTable outputBuffer
         else if executeCode.Chars(instructionP) = '<' || executeCode.Chars(instructionP) = '>' then
            ExecutionLoop (instructionP + 0x1) (NewMemoryP instructionP memoryP memory) memory loopTable outputBuffer
         else
            ExecutionLoop (instructionP + 0x1) memoryP memory loopTable outputBuffer

   ExecutionLoop 0x0 0x0 (0x0uy |> List.replicate(0x8000) |> List<Byte>) (GetLoops(executeCode)) ""

//This function is executed when this program is started.
[<EntryPoint>]
let Main argv = 
   if argv.GetUpperBound(0) = 2 then
      Execute (File.ReadAllText(argv.[0])) (Unescape(argv.[1])) (Unescape(argv.[2]))
   else      
      printfn "%s" ("Usage: \"" + Path.GetFileName(ProgramInformation.FileName) + "\" code_file input_line_break output_line_break")
   0