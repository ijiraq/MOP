

FFLAGS = -fPIC
LDFLAGS = -lcfitsio

SRCS = caldat.f check_version.f close_image.f convol.f create_header.f fit_util.f get_header.f match_stars_util.f match_two_cats.f momenta.f open_image.f read_center.f read_file_name.f read_seg.f readq.f segment.f select_stars_util.f sort.f thresh.f var_thresh.f

OBJS = $(SRCS:.f=.o)
PROGRAMS = stepZjmp step0jmp step1jmp step2ajmps step2ajmpl step2bjmp step3jmp
lib_stat = libjmp.a

all: stepZjmp step0jmp step1jmp step2ajmps step2ajmpl step3jmp

libjmp.a: $(OBJS)
	ar rv libjmp.a $(OBJS)
	ranlib libjmp.a

stepZjmp: stepZjmp.f libjmp.a
	$(FC) $^ $(LDFLAGS) -o $@

step0jmp: step0jmp.f libjmp.a
	$(FC) $^ $(LDFLAGS) -o $@

step1jmp: step1jmp.f libjmp.a
	$(FC) $^ $(LDFLAGS) -o $@

step2ajmps: step2ajmps.f libjmp.a
	$(FC) $^ $(LDFLAGS) -o $@

step2ajmpl: step2ajmpl.f libjmp.a
	$(FC) $^ $(LDFLAGS) -o $@

step2bjmp: step2bjmp.f libjmp.a
	$(FC) $^ $(LDFLAGS) -o $@

step3jmp: step3jmp.f libjmp.a
	$(FC) $^ $(LDFLAGS) -o $@

.f.o:
	$(FC) -c $< -o $@

clean :
	rm $(OBJS) $(PROGRAMS) libjmp.a

install:
	install -t $(BINDIRA) $(PROGRAMS)
	install -t $(LIBDIRA) libjmp.a
