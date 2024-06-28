using System.Diagnostics;

namespace pysharp;

internal class Program
{
    public static void Main()
    {
        string code = File.ReadAllText(@"C:\Users\HarrisonO’Leary\RiderProjects\pysharp\hi.pys").Replace("\r", "").Replace("\t", "    ");
        
        Interpreter interpreter = new();
        string errorMessage = interpreter.Interpret(code);
        Console.WriteLine("\n\\--------------\\\nMessage:\n" + errorMessage);
    }
}