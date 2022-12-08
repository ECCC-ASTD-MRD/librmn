void monexit(int err)
{
   Lib_Log(APP_LIBRMN,APP_INFO,"%f: sortie cote jardin code %d\n",__func__,err);
}

main()
{
  set_user_exit_handler(monexit);
  user_exit_handler(15);
  user_exit_handler(22);
}
