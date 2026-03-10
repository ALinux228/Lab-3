open System
open System.IO

// Проверка диапазона (цифра)
let rec validDigit () =

    let input = Console.ReadLine()

    match 
        Int32.TryParse(input) 
    with
    | true, number when number >= 0 && number <= 9 -> 
        number
    | _ -> 
        printfn "Введены неверные данные. Повторите ввод (0-9): "
        validDigit()

// Создание последовательности
let rec createSeq () =
    printf "Введите целые числа через пробел: "
    let input = Console.ReadLine()
    
    let elements = 
        input.Split(' ') 
        |> Array.filter (fun s -> s.Trim() <> "")
        |> Array.toSeq 
    
    let flag, validValues = 
        elements
        |> Seq.fold (fun (isValid, acc) s ->
            match Int32.TryParse(s.Trim()) with
            | true, value -> 
                (isValid, Seq.append acc (Seq.singleton value))
            | false, _ -> 
                printfn "Ошибка: '%s' не является целым числом" s
                (false, acc)  
        ) (true, Seq.empty<int>) 
    
    if flag then
        validValues 
    else
        printfn "Введите данные заново"
        createSeq()

// Последовательность последних цифр
let lastDigits (numbers) =
    numbers
    |> Seq.map (fun x -> abs x % 10)

// Сумма чисел с заданной последней цифрой
let sumLastDigit (numbers) digit =
    numbers 
    |> Seq.fold (fun acc x -> 
        if abs x % 10 = digit 
        then acc + x
        else acc) 0

let findNonTXT (rootDirectory: string) : seq<string> =
    let rec getAllFiles (dir: string) = seq {
        try
            // Получение файлов в текущем каталоге
            let files = Directory.GetFiles(dir)
            for file in files do
                let extension = Path.GetExtension(file).ToLower()
                if extension <> ".txt" then
                    yield file
            
            // Получаем подкаталоги
            let subDirs = Directory.GetDirectories(dir)
            for subDir in subDirs do
                yield! getAllFiles subDir
        with
        | :? UnauthorizedAccessException ->
            printfn "Нет доступа к каталогу: %s" dir
            yield! Seq.empty
        | ex ->
            printfn "Ошибка при доступе к %s: %s" dir ex.Message
            yield! Seq.empty
    }
    
    if Directory.Exists(rootDirectory) then
        getAllFiles rootDirectory
    else
        printfn "Ошибка: Каталог '%s' не существует" rootDirectory
        Seq.empty

[<EntryPoint>]
let main _ =
    printfn "Выберите действие:"
    printfn "1. Создать последовательность (последние цифры чисел)"
    printfn "2. Вычислить сумму чисел, оканчивающихся заданной цифрой" 
    printfn "3. Работа с файловой системой"  
    printfn "0. Выход"
    let ch = int (Console.ReadLine())
    match ch with
    | 1 ->
        let seqNumbers = createSeq()
        printfn "Введенная последовательность: %A" seqNumbers
        let seqLast = lastDigits seqNumbers
        printfn "Последовательность последних цифр: %A" seqLast

    | 2 ->
        let seqNumbers = createSeq()
        printfn "Введенная последовательность: %A" seqNumbers
        printf "Введите цифру: "
        let digit = validDigit()
        let sumLast = sumLastDigit seqNumbers digit
        printfn "Сумма чисел: %A" sumLast 
    | 3 -> 
        printf "Введите путь к каталогу: "
        let rootDir = Console.ReadLine()
        let result = findNonTXT rootDir
        printfn "Последовательность файлов: %A" result 
    | 0 ->
        printf "Программа завершена."

    | _ -> 
        printf "Повторите ввод"

    0
