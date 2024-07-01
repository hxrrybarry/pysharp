namespace pysharp;

public static class StaticInbuilts
{
    public static object Print(object[] args)
    {
        string text = args[0].ToString();

        Console.WriteLine(text);
        return true;
    }

    public static object Stringify(object[] args)
    {
        object trueValue = args[0];
        string stringValue = trueValue.ToString();

        if (trueValue is List<object> list)
        {
            stringValue = '[' + string.Join(", ", list.Select(x => x.ToString()).ToArray()) + ']';
        }
        
        else if (trueValue is Dictionary<object, object> dict)
        {
            stringValue = '{' + string.Join(", ", dict.Select(kv => $"{kv.Key}: {kv.Value}")) + '}';
        }

        return stringValue;
    }

    public static object Sleep(object[] args)
    {
        try
        {
            int delay = (int)args[0];
            string factor = (string)args[1];
            switch (factor)
            {
                case "s": delay *= 1000; break;
                case "m": delay *= 60_000; break;
                case "h": delay *= 3_600_000; break;
                default: break;
            }

            Thread.Sleep(delay);
            return true;
        }
        catch { return null; }
    }

    public static object Abs(object[] args)
    {
        try
        {
            float n = Convert.ToSingle(args[0]);
            return MathF.Abs(n);
        }
        catch { return null; }
    }

    public static object Root(object[] args)
    {
        try
        {
            float x = Convert.ToSingle(args[0]);
            float n = Convert.ToSingle(args[1]);

            return MathF.Pow(x, 1 / n);
        }
        catch { return null; }
    }

    public static object Sin(object[] args)
    {
        try
        {
            float n = Convert.ToSingle(args[0]);
            return MathF.Sin(n);
        }
        catch { return null; }
    }

    public static object Cos(object[] args)
    {
        try
        {
            float n = Convert.ToSingle(args[0]);
            return MathF.Cos(n);
        }
        catch { return null; }
    }
    public static object Tan(object[] args)
    {
        try
        {
            float n = Convert.ToSingle(args[0]);
            return MathF.Tan(n);
        }
        catch { return null; }
    }

    public static object Asin(object[] args)
    {
        try
        {
            float n = Convert.ToSingle(args[0]);
            return MathF.Asin(n);
        }
        catch { return null; }
    }

    public static object Acos(object[] args)
    {
        try
        {
            float n = Convert.ToSingle(args[0]);
            return MathF.Acos(n);
        }
        catch { return null; }
    }

    public static object Atan(object[] args)
    {
        try
        {
            float n = Convert.ToSingle(args[0]);
            return MathF.Atan(n);
        }
        catch { return null; }
    }

    public static object Length(object[] args)
    {
        object trueValue = args[0];

        return trueValue switch
        {
            List<object> list => list.Count,
            Dictionary<object, object> dict => dict.Count,
            _ => Convert.ToString(trueValue).Length
        };
    }

    public static object RandomInt(object[] args)
    {
        if (int.TryParse(args[0].ToString(), out int min) && int.TryParse(args[1].ToString(), out int max))
        {
            return new Random().Next(min, max);
        }

        throw new Exception($"Error: function 'randint(min, max)' takes in integers.");
    }
    
    public static object ReadFile(object[] args)
    {
        string path = Convert.ToString(args[0]);

        if (File.Exists(path)) { return File.ReadAllText(path); }

        throw new Exception($"Error: file '{path}' does not exist.");
    }

    public static object WriteFile(object[] args)
    {
        string path = Convert.ToString(args[0]);
        string text = Convert.ToString(args[1]);

        File.WriteAllText(path, text);
        return true;
    }
}