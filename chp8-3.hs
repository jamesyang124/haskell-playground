{-- Recursive DataStructure --}

{--

As we've seen, a constructor in an algebraic data type can have several (or none at all) fields and each field must be of some concrete type. With that in mind, we can make types whose constructors have fields that are of the same type!

Using that, we can create recursive data types, where one value of some type contains values of that type, which in turn contain more values of the same type and so on.

--}
