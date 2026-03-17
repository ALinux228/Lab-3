open System
open System.IO

// Проверка диапазона (цифра)
let rec validDigit () =

    let input = Console.ReadLine()

    match Int32.TryParse(input) with
    | true, number when number >= 0 && number <= 9 -> 
        number
    | _ -> 
        printfn "Введены неверные данные. Повторите ввод (0-9): "
        validDigit()

let rec validNumber () =
    printf "Введите число: "
    let input = Console.ReadLine()

    match Int32.TryParse(input) with
    | true, number when number >= -999999999 && number <= 999999999 -> 
        number
    | _ -> 
        printfn "Введены неверные данные. Повторите ввод: "
        validNumber()

// Создание последовательности
let createSeq () =
    printf "Введите длину последовательности: \n"
    let n = validNumber ()
    let numbers = seq {for i in 1..n do yield validNumber()}
    numbers

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
        printfn "Нахождение последовательности последних цифр"
    
        let numbers = createSeq() 
    
        let result = 
            numbers 
            |> Seq.map (fun x -> abs x % 10) 
            |> Seq.toList
    
        printfn "Последние цифры: %A" result

    | 2 ->
        let numbers = createSeq()
    
        printfn "Введите цифру (0-9): "
        let digit = validDigit()
    
        let result = 
            Seq.fold 
                (fun sum x -> 
                    if abs x % 10 = digit then
                        sum + x 
                    else 
                        sum) 
                0 numbers
    
        printfn "Сумма элементов, оканчивающихся на %d: %d" digit result
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
