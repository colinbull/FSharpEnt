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