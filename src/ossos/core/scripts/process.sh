#!/bin/bash


exp1=${1}
exp2=${2}
exp3=${3}
main=$(pwd)

# move into a working directory named for the field and chipnum
field=$(gethead ${exp1} OBJECT)
ccdnum=$(gethead ${exp1} T_SDOID)
detnum=$(gethead ${exp1} DET-ID)
mkdir -p ${field}/${detnum} || exit 1
cd ${field}/${detnum} || exit 1

# link the images into the working directory
[ -f ${exp1} ] || ln -s ${main}/${exp1} ./
[ -f ${exp2} ] || ln -s ${main}/${exp2} ./
[ -f ${exp3} ] || ln -s ${main}/${exp3} ./

min_rate=${min_rate=0.5}
max_rate=${max_rate=10.0}
angle=${angle=-23}
width=${width=30}
plant_width=${plant_width=20}
num=${num=60}
loops=${loops=15}

_SEX_THRESHOLD=1.1
_WAVE_THRESHOLD=1.7
_FWHM=4.0
_MAX_COUNT=30000


imgs=( $(extract_hsc $exp1) $(extract_hsc $exp2) $(extract_hsc $exp3) )
expnums=()
echo "${imgs[@]}"
for img in "${imgs[@]}";
do
  echo ${img}
  beeid=$(gethead ${img}.fits T_BEEID)
  expnum=$(gethead ${img}.fits EXP-ID | sed 's/HSCE//' | awk ' { printf("%d", substr($1, 1, 8)) }')
  let expnum=expnum+beeid
  expnums+=( ${expnum} )
done

function rotate_angle {
  beeid=$(gethead "${1}.fits" T_BEEID)
  sdoid=$(gethead "${1}.fits" T_SDOID)
  dx=$(echo "${2}"|awk '{print cos($1*3.14159/180) }' )
  dy=$(echo "${2}"|awk '{print sin($1*3.14159/180) }' )
  if [ $beeid -eq 0 ]; then
    if [ "${sdoid}" -eq 35 ] || [ "$sdoid" -eq 31 ]; then
            dxp=$(echo "${dx}" | awk ' { print -1*$1 } ')
            dyp=${dy}
    else  # CCD is not flipped
            dxp=${dy}
            dyp=${dx}
    fi
  else
    if [ "$sdoid" -eq 35 ] || [ "$sdoid" -eq 31 ]; then
      dxp=${dx}
      dyp=$(echo "${dy}" | awk ' { print -1*$1 } ' )
    else  # CCD is not flipped
      dyp=$(echo "${dx}" | awk ' { print -1*$1 } ' )
      dxp=$(echo "${dy}" | awk ' { print -1*$1 } ' )
    fi
  fi
  echo "${dxp}" "${dyp}"|awk '{print(atan2($2,$1)*180/3.14159)}'
}
angle=$(rotate_angle "${imgs[0]}" ${angle})

# build the PSF and get the source list.
prefix=""
function do_search {
  for img in  "${imgs[@]}";
    do
    stepZjmp -f ${preix}${img}
    jmpmakepsf.csh ./ ${preix}${img} yes yes
    fwhm=$(cat ${preix}${img}.fwhm)
    rm -f weight.fits
    ln -s ${img}_weight.fits weight.fits
    step1jmp -f ${preix}${img} -w ${fwhm} -m ${_MAX_COUNT} -t ${_WAVE_THRESHOLD}
    step1matt -f ${preix}${img} -w ${fwhm} -m ${_MAX_COUNT} -t ${_SEX_THRESHOLD}
  done

  # Determine lists of non-stationary sources.
  step2jmp ${preix}${imgs[0]} ${preix}${imgs[1]} ${preix}${imgs[2]}
  step2matt_jmp -f1 ${preix}${imgs[0]} -f2 ${preix}${imgs[1]} -f3 ${preix}${imgs[2]}

  # link non-stationary source lists into moving objects.
  step3jmp -f1 ${preix}${imgs[0]} -f2 ${preix}${imgs[1]} -f3 ${preix}${imgs[2]} -a ${angle} -w ${width} -rn ${min_rate} -rx ${max_rate}
  step3matt -f1 ${preix}${imgs[0]} -f2 ${preix}${imgs[1]} -f3 ${preix}${imgs[2]} -a ${angle} -w ${width} -rn ${min_rate} -rx ${max_rate}

  # make a combined candidate list.
  comb-list ${preix}${imgs[0]}
  comb_to_astrom ${preix}${imgs[0]}

}

do_search

# add artificiaal sources and determine detection efficiency.
align_novos "${expnums[@]}" --ccd ${ccdnum} -v --type p
for i in $(seq 1 ${loops});
do
  plant_novos "${expnums[@]}" --ccd ${ccdnum} -v --type p --rmin ${min_rate} --rmax ${max_rate} \
    --ang "${angle}" --width ${plant_width} --num ${num}
  preix="fk"
  do_search
  ccdstr=$(echo ${ccdnum}|awk '{printf("%02d",$1)}')
  astrom_mag_check_novos "${expnums[0]}" "${ccdnum}" --expnum "${expnums[0]}" --astrom-filename "fk${expnums[0]}p${ccdstr}.measure3.cands.astrom" --fk --type p
done


# Done
cd ${main} || exit 1
