// The printed 'b' value should be the one declared in a block above.
{
    val b = 5;
    {
        print b;
    }
}
{
    val b = 6;
    {
        {
            print b;
        }
    }
}