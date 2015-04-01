void monexit(int err)
{
  printf(" sortie cote jardin code %d\n",err);
}

main()
{
  set_user_exit_handler(monexit);
  user_exit_handler(15);
  user_exit_handler(22);
}
