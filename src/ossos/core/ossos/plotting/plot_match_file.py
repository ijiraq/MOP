import matplotlib

matplotlib.use("Agg")
from matplotlib.backends.backend_pdf import PdfPages
from matplotlib import rcParams
from matplotlib import pyplot as plt
from matplotlib import gridspec
import numpy
import sys
from astropy.table import Table
import logging


def main():
    """
    Given a set of match files, plot the MOP detection efficiency and flux calibration.

    Result is a PDF with one efficiency plot per file per page of the PDF.
    """
    # # EmulateApJ columnwidth=245.26 pts
    fig_width_pt = 546.0 * 3.0
    inches_per_pt = 1.0 / 72.27
    golden_mean = (numpy.sqrt(5.) - 1.0) / 2.0
    fig_width = fig_width_pt * inches_per_pt
    fig_height = fig_width * golden_mean * 1.5
    fig_size = [fig_width, fig_height]

    params = {'backend': 'pdf',
              'font.size': 21,
              'text.usetex': False,
              'image.aspect': 'equal',
              'figure.subplot.left': 0.2,
              'figure.subplot.bottom': 0.15,
              'figure.figsize': fig_size}
    rcParams.update(params)

    pp = PdfPages('eff.pdf')
    logging.basicConfig(level=logging.DEBUG)
    for match_file in sys.argv[1:]:
        plt.clf()
        gs = gridspec.GridSpec(2, 2)
        ax_frac = plt.subplot(gs[0, 0])
        ax_xy = plt.subplot(gs[0, 1])
        ax_angle = plt.subplot(gs[1, 0])
        gs2 = gridspec.GridSpecFromSubplotSpec(3, 1, subplot_spec=gs[1, 1])
        ax_dmag = {'measure_mag1': plt.subplot(gs2[0, 0]),
                   'measure_mag2': plt.subplot(gs2[1, 0]),
                   'measure_mag3': plt.subplot(gs2[2, 0])}

        T = Table.read(match_file, format='ascii.ecsv')
        # discovery requires a measured magnitude in one of the three exposures
        T['found'] = numpy.any([~T['measure_mag1'].mask, ~T['measure_mag2'].mask, ~T['measure_mag3'].mask], axis=0)

        plt.suptitle(match_file)
        # One line on the efficiency plot per sky_rate bin
        rate_bins = [{'lower': 0.5, 'upper': 1},
                     {'lower': 1.0, 'upper': 5.0},
                     {'lower': 5.0, 'upper': 15}]

        for rate_bounds in rate_bins:
            mask1 = (T['sky_rate'] > rate_bounds['lower']) & (T['sky_rate'] < rate_bounds['upper'])
            (nadd, bins) = numpy.histogram(T['mag'][mask1], bins=numpy.arange(21, 26, 0.1))
            mask2 = numpy.all([mask1, T['found']], axis=0)
            (nfnd, bins) = numpy.histogram(T['mag'][mask2], bins=numpy.arange(21, 26, 0.1))
            f = nfnd[nadd > 0] / (1.0 * nadd[nadd > 0])
            m = bins[0:-1][nadd > 0]
            ax_frac.plot(m, f, 'o-', label=f"{rate_bounds['lower']:3.1f}:{rate_bounds['upper']:3.1f}")

        ax_frac.set_xlabel('mag')
        ax_frac.set_ylabel('f')
        ax_frac.set_xlim(21.5, 25.5)

        # skip for proposal plot
        if False:
            for idx in range(len(rate_bins) - 1):
                mask1 = numpy.all([rate_bins[idx] < T['sky_rate'],
                                   T['sky_rate'] < rate_bins[idx + 1],
                                   T['x'] < 1500,
                                   T['x'] > 500,
                                   T['y'] < 4000,
                                   T['y'] > 500], axis=0)
                mask2 = numpy.all([mask1, T['measure_mag1'].mask == False], axis=0)
                (nadd, bins) = numpy.histogram(T['mag'][mask1], bins=numpy.arange(21, 26, 0.25))
                (nfnd, bins) = numpy.histogram(T['mag'][mask2], bins=numpy.arange(21, 26, 0.25))
                f = nfnd[nadd > 0] / (1.0 * nadd[nadd > 0])
                m = bins[0:1][nadd > 0]
                # ax_frac.plot(m, f, 's--', label="{:3.1f}:{:3.1f}".format(rates[idx], rates[idx+1]))

        ax_frac.legend(loc=3)

        ax_xy.hexbin(T['x'], T['y'], cmap=plt.cm.Blues, mincnt=1, gridsize=80)
        ax_xy.hexbin(T['x'][~T['found']], T['y'][~T['found']], cmap=plt.cm.Reds,
                     alpha=1, mincnt=1, gridsize=80)
        ax_xy.set_xlabel('X-pixel')
        ax_xy.set_ylabel('Y-pixel')
        ax_xy.set_xlim(min(0, T['x'].min()), max(2048, T['x'].max()))
        ax_xy.set_ylim(min(0, T['y'].min()), max(4176, T['y'].max()))

        dx = T['sky_rate'] * numpy.cos(numpy.radians(T['angle']))
        dy = T['sky_rate'] * numpy.sin(numpy.radians(T['angle']))
        ax_angle.hexbin(dx, dy, cmap=plt.cm.Blues, mincnt=1, gridsize=40)
        ax_angle.hexbin(dx[T['found']], dy[T['found']], alpha=1,
                        cmap=plt.cm.Reds, mincnt=1, gridsize=40)
        ax_angle.axis([dx.min()*0.95, dx.max()*1.05, dy.min()*0.95, dy.max()*1.05])
        ax_angle.set_xlabel('sky_rate*cos(angle)')
        ax_angle.set_ylabel('sky_rate*sin(angle)')

        for measure in ['measure_mag1', 'measure_mag2', 'measure_mag3']:
            dmag = T['mag'] - T[measure]
            ax_dmag[measure].hexbin(T['mag'], dmag, cmap=plt.cm.Blues, gridsize=60)
            ax_dmag[measure].axis([21, 25, -0.25, 0.25])
        ax_dmag['measure_mag3'].set_xlabel('planted_mag')
        ax_dmag['measure_mag2'].set_ylabel('plant - measure')

        pp.savefig()

    pp.close()


if __name__ == '__main__':
    main()
