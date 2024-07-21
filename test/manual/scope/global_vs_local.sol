val c = 7; // Global declaration
{
    {
        {
            print c; // Global access when not in global scope
        }
    }
}