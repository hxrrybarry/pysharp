using System.Text.RegularExpressions;

namespace pysharp;

internal partial class Program
{
    private static string NormalizeNewlines(string input)
    {
        // replace multiple consecutive newline characters with a single newline
        // this means the only lines the interpreter has to deal with contain something valid
        input = input.Replace("\r", "").Replace("\t", "");
        return MyRegex().Replace(input, "\n");
    }

    public static void Main()
    {
        string code = NormalizeNewlines(File.ReadAllText(@"C:\Users\HarrisonO’Leary\RiderProjects\pysharp\game.pys"));

        Interpreter interpreter = new();
        string errorMessage = interpreter.Interpret(code);
        Console.WriteLine($"\n\\_______________\\\nMessage:\n{errorMessage}");
    }
    
    [GeneratedRegex(@"\n+")]
    private static partial Regex MyRegex();
}