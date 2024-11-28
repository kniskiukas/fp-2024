newtype Parser a = Parser {
    runParser :: String -> Either String (a, String)
}

parseQuery :: String -> Either String Query
-- parseQuery s = case or2 parseAddRequest (or2 parseListRequests (or2 parseRemoveRequest (or2 parseUpdateRequest (or2 parseFindRequest parseRemoveAllRequests))))
--   s of
--     Left _ -> Left ("Unexpected command: " ++ s)
--     Right (v, r) -> if null r then Right v else Left ("Unexpected '" ++ r ++ "' after parsing")
parseQuery s = case runParser (parseAddRequest <|> parseListRequests <|> parseRemoveRequest <|> parseUpdateRequest <|> parseFindRequest <|> parseRemoveAllRequests) s of
  Left err -> Left err
  Right (result, rest) -> if null rest then Right result else Left ("Unexpected '" ++ rest ++ "' after parsing")

parseAddRequest :: Parser Query
-- parseAddRequest = and3 (\_ _ r -> AddRequest r) (parseString "add_request") parseSpace parseRequest

parseAddRequest =  AddRequest <$> (parseString "add_request" *> parseSpace *> parseRequest)

-- >>> runParser parseRequest "1,abc,def,ghi,jkl,mno,pqr"
-- Right (Request {requestId = 1, requestType = "abc", requestOrigin = "def", items = Items ["ghi"]},",jkl,mno,pqr")
-- >>> runParser parseRequest "1,abc,def,ghi"
-- Right (Request {requestId = 1, requestType = "abc", requestOrigin = "def", items = Items ["ghi"]},"")

parseListRequests :: Parser Query
-- parseListRequests s = case parseString "list_requests" s of
--   Left err -> Left err
--   Right (_, r) -> Right (ListRequests, r)

parseListRequests = ListRequests <$ parseString "list_requests"

parseRemoveRequest :: Parser Query
parseRemoveRequest = RemoveRequest <$> (parseString "remove_request" *> parseSpace *> parseRequestId)


parseUpdateRequest :: Parser Query
-- parseUpdateRequest = and5 (\_ _ n _ r -> UpdateRequest n r) (parseString "update_request") parseSpace parseRequestId parseSpace parseRequest
parseUpdateRequest = UpdateRequest <$> (parseString "update_request" *> parseSpace *> parseRequestId) <*> (parseSpace *> parseRequest)

parseFindRequest :: Parser Query
-- parseFindRequest = and3 (\_ _ n -> FindRequest n) (parseString "find_request") parseSpace parseRequestId
parseFindRequest = FindRequest <$> (parseString "find_request" *> parseSpace *> parseRequestId)

parseRemoveAllRequests :: Parser Query
-- parseRemoveAllRequests s = case parseString "remove_all_requests" s of
--   Left err -> Left err
--   Right (_, r) -> Right (RemoveAllRequests, r)

parseRemoveAllRequests = RemoveAllRequests <$ parseString "remove_all_requests"

