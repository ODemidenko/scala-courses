//Функция, которая проводит указанную операцию над всеми объектами списка
def foldRight[A](ls:List[A],reducer:(A,A)=>A, term:A)(a:A,b:A):A ={
  def iter(ls:List[A]):A =
    if (ls.isEmpty) term
    else reducer(ls.head,iter(ls.tail))
}