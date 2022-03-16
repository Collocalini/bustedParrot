# default.nix
{ compiler ? "ghc822" }:
let 
   
  pkgGhc822 = import (builtins.fetchTarball {
                     # Descriptive name to make the store path easier to identify                
                     name = "ghc822";                                                 
                     # url = "https://github.com/NixOS/nixpkgs/";   
                     url = "https://github.com/NixOS/nixpkgs/archive/72c48fef2ffd757b27b4ceab1787c1f9de1d20a1.tar.gz";
                     #ref = "refs/heads/nixpkgs-18.03-darwin";                     
                     #rev = "72c48fef2ffd757b27b4ceab1787c1f9de1d20a1";                                           
                 }) {}; 
                 
  pkgPandoc = import (builtins.fetchTarball {
                     name = "pandoc1.17.1";                                                 
                     url = "https://github.com/NixOS/nixpkgs/archive/8b9e2010b416083ad61e33117582f1733fafd883.tar.gz";
                 }) {}; 

  pkgs = import <nixpkgs> { inherit pkgGhc822; inherit pkgPandoc; };
  
in 
  {databaseHandling = pkgs.haskellPackages.callPackage ./databaseHandling.nix { };
  } 
    
  
  
