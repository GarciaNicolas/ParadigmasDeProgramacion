


autor :: (autor, titulo, paginas)   -> autor    
autor (autor,_,_)                    = autor    

titulo :: (autor, titulo, paginas)  -> titulo   -- >   ACCESSORS
titulo (_,titulo,_)                  = titulo   

paginas :: (autor, titulo, paginas) -> paginas 
paginas (_,_,paginas)                = paginas  


type Libro = (String, String, Int)
elVisitante :: Libro
elVisitante          = ("El visitante", "Stephen King", 592)
shingekinoKyojin1 :: Libro
shingekinoKyojin1    = ("Shingeki no Kyojin 1", "Hajime Isamaya", 40)
shingekinoKyojin3 :: Libro
shingekinoKyojin3    = ("Shingeki no Kyojin 3", "Hajime Isamaya", 40)
shingekinoKyojin127 :: Libro
shingekinoKyojin127  = ("Shingeki no Kyojin 127", "Hajime Isamaya", 40)
fundacion :: Libro
fundacion            = ("Fundacion", "Isaac Asimov", 230)
sandman5 :: Libro
sandman5             = ("Sandman 5", "Neil Gaiman", 35)
sandman10 :: Libro
sandman10            = ("Sandman 10", "Neil Gaiman", 35)
sandman12 :: Libro
sandman12            = ("Sandman 12", "Neil Gaiman", 35)
eragon :: Libro
eragon               = ("Eragon", "Christopher Paolini", 544)
eldest :: Libro
eldest               = ("Eldest", "Christopher Paolini", 704)
brisignr :: Libro
brisignr             = ("Brisignr", "Christopher Paolini", 700)
legado :: Libro
legado               = ("Legado", "Christopher Paolini", 811)


type Biblioteca = [Libro]
biblioteca :: Biblioteca
biblioteca = [elVisitante, shingekinoKyojin1, shingekinoKyojin3, shingekinoKyojin127, sandman5, sandman10, sandman12, eragon, eldest, brisignr, legado] 




promedioDePaginas :: Biblioteca -> Int
promedioDePaginas miBiblioteca = sumatoriaPaginasBiblioteca miBiblioteca `div` (length miBiblioteca)

sumatoriaPaginasBiblioteca :: Biblioteca -> Int
sumatoriaPaginasBiblioteca miBiblioteca = sum.(map paginas) miBiblioteca





lecturaObligatoria :: [Libro] -> Bool
lecturaObligatoria miBiblioteca = elem "Stephen King" (autores miBiblioteca) || elem "Isaac Asimov" (autores miBiblioteca) || elem "Christopher Paolini" (autores miBiblioteca)

autores :: [Libro] -> [String]
autores miBiblioteca = map segundo miBiblioteca


esFantasiosa :: [Libro] -> Bool
esFantasiosa miBiblioteca = elem "Christopher Paolini" (autores miBiblioteca) || elem "Neil Gaiman" (autores miBiblioteca)


nombreDeLaBiblioteca 

genero