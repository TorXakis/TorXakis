-- Two constructors to ensure that error can only be detected at ADT creation.
prop_SignatureClashes :: Gen Bool
prop_SignatureClashes =
    let cName = unsafeName "Cstr"
        cDef = unsafeConstructorDef cName []
        isCstrName = unsafeName (TorXakis.Language.toString (txsFuncNameIsConstructor cDef)) 
      in do
        ctx <- arbitraryTestSortContext
        s <- arbitrarySort ctx
        let field = FieldDef isCstrName s
            c2Name = unsafeName "AnotherCstr"
            c2Def  = unsafeConstructorDef c2Name [field]
            aName  = unsafeName "ADT"
          in
            case mkADTDef aName [cDef, c2Def] of
                Left _  -> return $ s == SortBool
                Right _ -> return $ s /= SortBool

spec :: Spec
spec =
  describe "Adding an ADT definition to a TorXakis context" $ do
        it "might fail to signature clashes" $ property prop_SignatureClashes
        it "might fail to keyword usage" $ property todo