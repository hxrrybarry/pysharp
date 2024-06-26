using System;
using System.Linq.Expressions;
using System.Linq.Dynamic.Core;
using System.Text.RegularExpressions;

namespace pysharp;

internal class Program
{
    public static void Main()
    {
        string code = File.ReadAllText(@"C:\Users\harib\source\repos\pysharp\hi.pys");
        code = code.Replace("\r", "").Replace("\t", "    ");

        Console.WriteLine("Code:\n" + code + "\n\n\nOutput:");

        Interpreter interpreter = new();
        string errorMessage = interpreter.Interpret(code);
        Console.WriteLine("\n\n\n" + "Message:\n" + errorMessage);
    }
}