namespace pysharp;

internal class Program
{
    public static void Main()
    {
        string code = File.ReadAllText(@"C:\Users\harib\source\repos\pysharp\hi.pys");
        code = code.Replace("\r", "").Replace("\t", "    ");
        
        Interpreter interpreter = new();
        string errorMessage = interpreter.Interpret(code);
        Console.WriteLine("\n\\--------------\\\nMessage:\n" + errorMessage);
    }
}