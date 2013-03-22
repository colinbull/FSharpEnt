namespace FSharp.Enterprise

module Security =
    
    module Permissons = 
        
        open System.Security.Principal

        let requiresAdmin adminF compensation = 
            let principal = new WindowsPrincipal(WindowsIdentity.GetCurrent())
            if principal.IsInRole WindowsBuiltInRole.Administrator then
                adminF()
            else
                compensation()

    module User = 
        
        open System.Security
        open System.Security.Principal
        open System.DirectoryServices.AccountManagement

        let genericPrincipal (userId:string) roles = 
            new GenericPrincipal(new GenericIdentity(userId), roles)

        let getUserContext (userName:string) (password:string) = 
            new UserPrincipal(new PrincipalContext(ContextType.Machine, null, userName, password))

        let validateUser (userName:string) (password:string) = 
            use ctx = new PrincipalContext(ContextType.Machine)
            ctx.ValidateCredentials(userName, password)

    module RSA =
        
        open System
        open System.Text
        open System.Security.Cryptography
                            
        let execute keyContainer (f : RSACryptoServiceProvider -> 'a) =
            if String.IsNullOrEmpty(keyContainer)
            then invalidOp("No Key Container set")
            else
                let cp = new CspParameters()
                cp.Flags <- CspProviderFlags.UseMachineKeyStore
                cp.KeyContainerName <- keyContainer
                use RSA = new RSACryptoServiceProvider(cp)
                f(RSA)           

        let deleteKeyContainer (container : string) = 
            let cp = new CspParameters()
            cp.Flags <- CspProviderFlags.UseMachineKeyStore
            cp.KeyContainerName <- container
            let rsa =new RSACryptoServiceProvider(cp)
            rsa.PersistKeyInCsp <- false
            rsa.Clear()
        
        let encrypt (bytes:byte[]) (rsa : RSACryptoServiceProvider)  = 
            rsa.Encrypt(bytes, false)

        let decrypt (bytes:byte[]) (rsa : RSACryptoServiceProvider)  =
            rsa.Decrypt(bytes, false)

        let encryptString (str:string) (rsa : RSACryptoServiceProvider) =
            let bytes = encrypt (Encoding.UTF32.GetBytes(str)) rsa
            let outgoing = Convert.ToBase64String(bytes)
            outgoing

        let decryptString (str:string) (rsa : RSACryptoServiceProvider) = 
            let bytes = Convert.FromBase64String(str)
            decrypt bytes rsa |> Encoding.UTF32.GetString
           

