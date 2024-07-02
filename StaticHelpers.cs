namespace pysharp;

public static class StaticHelpers
{
    // this allows for fully recursive arguments
    // ie: a string of "\"add\", Sum(27, Sub(56, 2)), 3, 8"
    // will correctly return an array of:
    /*
     * - "add"
     * - Sum(27, Sub(56, 2))
     * - 3
     * - 8
     */
    // and on the second index, the same algorithm will parse Sum(27, Sub(56, 2)) as:
    /*
     * - 27
     * - Sub(56, 2)
     */
    // and so on and so forth..
    public static string[] ParseArguments(string arguments)
    {
        var args = new List<string>();
        bool inQuotes = false;
        int depth = 0;
        int start = 0;

        for (int i = 0; i < arguments.Length; i++)
        {
            switch (arguments[i])
            {
                case '"':
                    inQuotes = !inQuotes;
                    break;
                case '(':
                case '[':
                    if (!inQuotes)
                    {
                        depth++;
                    }

                    break;
                case ')':
                case ']':
                    if (!inQuotes)
                    {
                        depth--;
                    }

                    break;
                case ',':
                    if (!inQuotes && depth == 0)
                    {
                        args.Add(arguments[start..i].Trim());
                        start = i + 1;
                    }

                    break;
            }
        }

        // add the last token
        args.Add(arguments[start..].Trim());

        return args.ToArray();
    }
}