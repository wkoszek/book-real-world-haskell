{-- snippet BookInfo --}
data BookInfo = Book Int String [String]
                deriving (Show)
{-- /snippet BookInfo --}

{-- snippet MagazineInfo --}
data MagazineInfo = Magazine Int String [String]
                    deriving (Show)
{-- /snippet MagazineInfo --}

{-- snippet BookReview --}
-- We will introduce the CustomerID type shortly.

data BookReview = BookReview BookInfo CustomerID String
{-- /snippet BookReview --}

{-- snippet BetterReview --}
type CustomerID = Int
type ReviewBody = String

data BetterReview = BetterReview BookInfo CustomerID ReviewBody
{-- /snippet BetterReview --}

{-- snippet BookRecord --}
type BookRecord = (BookInfo, BookReview)
{-- /snippet BookRecord --}

{-- snippet Customer --}
data Customer = Customer {
      customerID      :: CustomerID
    , customerName    :: String
    , customerAddress :: Address
    } deriving (Show)
{-- /snippet Customer --}

{-- snippet BillingInfo --}
type CardHolder = String
type CardNumber = String
type Address = [String]

data BillingInfo = CreditCard CardNumber CardHolder Address
                 | CashOnDelivery
                 | Invoice CustomerID
                   deriving (Show)
{-- /snippet BillingInfo --}

{-- snippet myInfo --}
myInfo = Book 9780135072455 "Algebra of Programming"
         ["Richard Bird", "Oege de Moor"]
{-- /snippet myInfo --}

{-- snippet customer1 --}
customer1 = Customer 271828 "J.R. Hacker"
            ["255 Syntax Ct",
             "Milpitas, CA 95134",
             "USA"]
{-- /snippet customer1 --}

{-- snippet customer2 --}
customer2 = Customer {
              customerID = 271828
            , customerAddress = ["1048576 Disk Drive",
                                 "Milpitas, CA 95134",
                                 "USA"]
            , customerName = "Jane Q. Citizen"
            }
{-- /snippet customer2 --}

{-- snippet ShoppingCart --}
data ShoppingCart = ShoppingCart CustomerID [BookInfo]
                    deriving (Show)
{-- /snippet ShoppingCart --}

{-- snippet accessors --}
bookID      (Book id title authors) = id
bookTitle   (Book id title authors) = title
bookAuthors (Book id title authors) = authors
{-- /snippet accessors --}

{-- snippet niceAccessors --}
nicerID      (Book id _     _      ) = id
nicerTitle   (Book _  title _      ) = title
nicerAuthors (Book _  _     authors) = authors
{-- /snippet niceAccessors --}
