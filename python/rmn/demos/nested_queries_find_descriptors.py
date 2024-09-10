import rmn
import argparse

p = argparse.ArgumentParser()
p.add_argument('-f', '--filename', required=True)
args = p.parse_args()

f = rmn.fst24_file(filename=args.filename)

for rec in f:
    if rec.nomvar in ['>>', '^^']:
        continue

    if rec.grtyp in ['A', '...']:
        print(f"Record {rec} has doesn't have '>>','^^'")
        continue

    try:
        q_tic_tic = f.new_query(nomvar='>>', ip1=rec.ig1, ip2=rec.ig2, ip3=rec.ig3)
        tic_tic = next(q_tic_tic)  # Same as Fortran `call q_desc%find_next(tic_tic)`
    except StopIteration:
        print(f"No tic-tic for {rec}")
        continue

    # Same as above but in one line
    try:
        tac_tac = next(f.new_query(nomvar='^^', ip1=rec.ig1, ip2=rec.ig2, ip3=rec.ig3))
    except StopIteration:
        print(f"No tac-tac for {rec}")
        continue

    print(f"Record {rec}")
    print(f"    >>: {tic_tic}")
    print(f"    ^^: {tac_tac}")
