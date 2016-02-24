__author__ = 'Michele Bannister   git:@mtbannister'

import sys

import matplotlib.pyplot as plt
import matplotlib.lines as mlines
import numpy
from astropy import units

import palettable
from ossos.planning.plotting import parsers, parameters, plot_fanciness


def full_aei(data_release, icut=False, aiq=False):
    fig, ax = plt.subplots(3, 1, sharex=True, figsize=(7, 8))  # a4 is 1 x sqrt(2), so use those proportions
    fig.subplots_adjust(hspace=0.25)

    ymin = -0.001
    orbit_classes = ['cla', 'res', 'sca', 'det',]#, 'cen']

    if parameters.RELEASE_VERSION == 4:
        # don't want uncharacterised for this plot
        data = data_release[numpy.array([name.startswith("o") for name in data_release['object']])]
        imax = 55
        emax = .65
        qmin = data['peri'].min() - 4
        qmax = data['peri'].max() + 4
        xinner = 25
        xouter = 90
        annotation = False
        e45_annotation = False

    if parameters.RELEASE_VERSION == 6:
        imax = 55
        emax = .85
        xinner = 15
        xouter = 90
        annotation = False
        e45_annotation = False
        # let's trim off anything with super-large error bars as well
        data = data_release[numpy.array([nobs > 7 for nobs in data_release['nobs']])]
        # data = data_release[numpy.array([name.startswith("o4h") for name in data_release['object']])]


    data['peri'] = data['a'] * (1. - data['e'])
    data['peri_E'] = (data['a_E'] / data['a']) + (data['e_E'] / data['e'])

    data.sort('cl')

    col = palettable.wesanderson.Zissou_5.mpl_colors[0:3] + ['0.6'] + ['1.0']
    # palettable.wesanderson.Zissou_5.mpl_colors[4:]

    coldcol = col
    hotcol = col[2]
    e45col = col #[palettable.wesanderson.Moonrise5_6.mpl_colors[4]]
    ms = 8
    cold_alpha = 0.8  # when plotting blue only  # 0.7
    hot_alpha = 0.25
    grid_alpha = 0.2
    ebarcolor = 'k'  # error bar colour: 0.1 is greyscale
    capsize = 1  # error bar cap width
    fmt = '.'

    sans_o3e45 = data[numpy.where(data['object'] != 'o3e45')]
    if icut:
        cold = sans_o3e45[numpy.where(sans_o3e45['i'] < 5.)]
        hot = sans_o3e45[numpy.where(sans_o3e45['i'] >= 5.)]
    else:  # no splits whatsoever
        cold = sans_o3e45

    # want to show o3e45 with a different symbol.
    o3e45 = data[numpy.where(data['object'] == 'o3e45')]

    ax = helio_i(ax, cold, fmt, cold_alpha, ebarcolor, capsize, ms, grid_alpha, coldcol, 1, orbit_classes, annotation=annotation)
    if icut:
        ax = helio_i(ax, hot, fmt, hot_alpha, ebarcolor, capsize, ms, grid_alpha, hotcol, 0, orbit_classes, annotation=annotation)
    ax = helio_i(ax, o3e45, '*', 0.7, ebarcolor, capsize, ms, grid_alpha, e45col, 2, ['cla'], annotation=e45_annotation)
    ax[0].set_ylim([ymin, imax])

    ax = a_i(ax, cold, fmt, cold_alpha, ebarcolor, capsize, ms, grid_alpha, coldcol, 1, orbit_classes, annotation=annotation)
    if icut:
        ax = a_i(ax, hot, fmt, hot_alpha, ebarcolor, capsize, ms, grid_alpha, hotcol, 0, annotation=annotation)
    ax = a_i(ax, o3e45, '*', 0.7, ebarcolor, capsize, ms, grid_alpha, e45col, 2, ['cla'], annotation=e45_annotation)
    ax[1].set_ylim([ymin, imax])

    if aiq:
        ax = a_q(ax, cold, fmt, cold_alpha, ebarcolor, capsize, ms, grid_alpha, coldcol, '$i < 5^{\circ}$',
                 1, annotation=annotation)
        if icut:
            ax = a_q(ax, hot, fmt, hot_alpha, ebarcolor, capsize, ms, grid_alpha, hotcol, '$i \geq 5^{\circ}$',
                     0, annotation=annotation)
        ax = a_q(ax, o3e45, '*', 0.7, ebarcolor, capsize, ms, grid_alpha, e45col, '', 2, annotation=e45_annotation)
        ax[2].set_ylim([qmin, qmax])
    else:
        if not icut:
            label = '$i < 5^{\circ}$'
        ax = a_e(ax, cold, fmt, cold_alpha, ebarcolor, capsize, ms, grid_alpha, coldcol, label,
                 1, orbit_classes, annotation=annotation)
        if icut:
            ax = a_e(ax, hot, fmt, hot_alpha, ebarcolor, capsize, ms, grid_alpha, hotcol, '$i \geq 5^{\circ}$',
                     0, orbit_classes, annotation=annotation)
        ax = a_e(ax, o3e45, '*', 0.7, ebarcolor, capsize, ms, grid_alpha, e45col, '', 2, ['cla'], annotation=e45_annotation)
        ax[2].set_ylim([ymin, emax])

    plt.xlim([xinner, xouter])
    ax[0].set_xticks(range(xinner, xouter, 5))
    ax[1].set_xticks(range(xinner, xouter, 5))
    ax[2].set_xticks(range(xinner, xouter, 5))

    plot_fanciness.remove_border(ax[0])
    plot_fanciness.remove_border(ax[1])
    plot_fanciness.remove_border(ax[2])

    resonances(ax, imax, emax)

    handles, labels = ax[2].get_legend_handles_labels()
    handles.append(mlines.Line2D([], [], marker='*', color=col[0], alpha=cold_alpha, linestyle=None))
    labels = ['classical', 'resonant', 'scattering', 'detached', 'o3e45']
    ax[0].legend(handles, labels, loc='upper right', numpoints=1, fontsize='small')

    plt.xlabel('semimajor axis (AU)')

    plt.draw()
    if aiq:
        type = 'aiq'
    else:
        type = 'aei'
    outfile = 'OSSOS_v{}_discoveries_{}_{}-{}_ilabel.pdf'.format(parameters.RELEASE_VERSION, type, xinner, xouter)
    plt.savefig(outfile, transparent=True, bbox_inches='tight')
    sys.stdout.write('{}\n'.format(outfile))


def helio_i(ax, data, fmt, alpha, ebarcolor, capsize, ms, grid_alpha, colour, zorder, orbit_classes, annotation=False):
    for i, orbclass in enumerate(orbit_classes):
        tnos = data[numpy.where(data['cl'] == orbclass)]
        ax[0].errorbar(tnos['dist'], tnos['i'],
                       xerr=tnos['dist_E'], yerr=tnos['i_E'],
                       zorder=zorder,
                       fmt=fmt, alpha=alpha, ecolor=ebarcolor, capsize=capsize, ms=ms, mfc=colour[i],
                       mec=colour[i], mew=0.5)

    ax[0].set_ylabel('inclination (degrees)')
    ax[0].grid(True, alpha=grid_alpha)
    ax[0].set_xlabel('heliocentric distance (AU)')

    if annotation:
        for obj in data:
            ax[0].annotate(obj['object'], (obj['dist'] + 0.7, obj['i']), size=5)

    return ax


def a_i(ax, data, fmt, alpha, ebarcolor, capsize, ms, grid_alpha, colour, zorder, orbit_classes, annotation=False):
    for i, orbclass in enumerate(orbit_classes):
        tnos = data[numpy.where(data['cl'] == orbclass)]
        ax[1].errorbar(tnos['a'], tnos['i'],
                       xerr=tnos['a_E'], yerr=tnos['i_E'],
                       zorder=zorder,
                       fmt=fmt, alpha=alpha, ecolor=ebarcolor, capsize=capsize, ms=ms, mfc=colour[i], mec=colour[i], mew=0.5)
    ax[1].set_ylabel('inclination (degrees)')
    ax[1].grid(True, alpha=grid_alpha)

    if annotation:
        for obj in data:
            ax[1].annotate(obj['object'], (obj['a'] + 0.7, obj['i']), size=5)

    return ax


def a_e(ax, data, fmt, alpha, ebarcolor, capsize, ms, grid_alpha, colour, label, zorder, orbit_classes, annotation=False):
    for i, orbclass in enumerate(orbit_classes):
        tnos = data[numpy.where(data['cl'] == orbclass)]
        label = orbclass
        ax[2].errorbar(tnos['a'], tnos['e'],
                       xerr=tnos['a_E'], yerr=tnos['e_E'],
                       zorder=zorder,
                       fmt=fmt, alpha=alpha, ecolor=ebarcolor, capsize=capsize, ms=ms, mfc=colour[i], mec=colour[i], mew=0.5,
                       label=label)
    ax[2].set_ylabel('eccentricity')
    ax[2].grid(True, alpha=grid_alpha)

    if annotation:
        for obj in data:
            ax[2].annotate(obj['object'], (obj['a'] + 0.7, obj['e']), size=5)

    return ax


def a_q(ax, data, fmt, alpha, ebarcolor, capsize, ms, grid_alpha, colour, label, zorder, annotation=False):
    ax[2].errorbar(data['a'], data['peri'],
                   xerr=data['a_E'], yerr=data['peri_E'],
                   zorder=zorder,
                   fmt=fmt, alpha=alpha, ecolor=ebarcolor, capsize=capsize, ms=ms, color=colour,
                   label=label)
    ax[2].set_ylabel('perihelion (AU)')
    ax[2].grid(True, alpha=grid_alpha)

    if annotation:
        for obj in data:
            ax[2].annotate(obj['object'], (obj['a'] + 0.7, obj['peri']), size=5)

    return ax


def resonances(ax, imax, emax):
    res_ids = ['1:1', '2:1', '3:2', '5:2', '7:3', '7:4', '5:3', '11:4', '8:5', '15:8', '13:5']
    a_N = 30.
    for res in res_ids:
        m, n = res.split(':')
        a_res = a_N * (float(m)/float(n))**(2/3.)
        ax[1].axvline(a_res, alpha=0.15, zorder=0)
        ax[1].annotate(res, (a_res, imax), rotation=90, ha='center', size='xx-small', alpha=0.7)
        ax[2].axvline(a_res, alpha=0.15, zorder=0)
        ax[2].annotate(res, (a_res, emax), rotation=90, ha='center', size='xx-small', alpha=0.7)

    return


def classicals_qi(data):
    # the top right panel i-q of Petit et al. only.
    data['peri'] = data['a'] * (1. - data['e'])
    data['peri_E'] = (data['a_E'] / data['a']) + (data['e_E'] / data['e'])

    o3e45 = data[numpy.where(data['object'] == 'o3e45')]
    classicals = data[numpy.where(data['p'] == 'm')]

    ms = 11
    capsize = 1  # error bar cap width
    alpha = 0.3
    fig, ax = plt.subplots(1, 1, figsize=(5, 5))
    ax.errorbar(classicals['i'], classicals['peri'],
                xerr=classicals['i_E'],
                yerr=classicals['peri_E'],
                fmt='.', alpha=alpha, ecolor='0.1', capsize=capsize, ms=ms)
    ax.errorbar(o3e45['i'], o3e45['peri'],
                xerr=o3e45['i_E'],
                yerr=o3e45['peri_E'],
                fmt='*', alpha=0.7, ecolor='0.1', capsize=capsize, ms=ms, color='r')

    ax.grid(True, alpha=0.4)
    ax.set_xlim([0., 35.])
    ax.set_xlabel('inclination (deg)')
    ax.set_xticks(range(5, 40, 10))
    ax.set_ylim([34., 48.])
    ax.set_yticks(range(36, 50, 4))
    ax.set_ylabel('perihelion (AU)')
    plot_fanciness.remove_border(ax)

    plt.draw()
    outfile = 'OSSOS_v{}_classical_iq.pdf'.format(parameters.RELEASE_VERSION)
    plt.savefig(outfile, transparent=True, bbox_inches='tight')



def classicals_aei(data):
    # designed to match the parameters and look of Petit et al. 2011
    # layout: top left, a-q, top right, i-q, lower left, a-i.
    peri = data['a'] * (1. - data['e'])

    ms = 5
    capsize = 2  # error bar cap width

    fig, ax = plt.subplots(2, 2, figsize=(10, 10))
    fig.subplots_adjust(hspace=0.0)  # want to work on the v-space as well

    ax[0][0].errorbar(data['a'], peri,
                      xerr=data['a_E'],
                      fmt='.', alpha=0.4, ecolor='0.1', capsize=capsize, ms=ms)
    ax[0][0].grid(True, alpha=0.4)
    ax[0][0].set_xlim([39., 48.])
    ax[1][0].set_xticks(range(40, 49, 2))
    ax[0][0].set_ylim([34., 48.])
    ax[0][0].set_ylabel('pericenter (AU)')
    ax[0][0].set_yticks(range(36, 50, 4))

    ax[0][1].errorbar(data['i'], peri,
                      xerr=data['i_E'],
                      fmt='.', alpha=0.4, ecolor='0.1', capsize=capsize, ms=ms)
    ax[0][1].grid(True, alpha=0.4)
    ax[0][1].set_xlim([0., 35.])
    ax[0][1].set_xlabel('inclination (deg)')
    ax[0][1].set_xticks(range(5, 40, 10))
    ax[0][1].set_ylim([34., 48.])
    ax[0][1].set_yticks(range(36, 50, 4))

    ax[1][0].errorbar(data['a'], data['i'],
                      xerr=data['a_E'], yerr=data['i_E'],
                      fmt='.', alpha=0.4, ecolor='0.1', ms=ms, capsize=capsize)
    ax[1][0].set_xlim([39., 48.])
    ax[1][0].set_xticks(range(40, 49, 2))
    ax[1][0].set_xlabel('semimajor axis (AU)')
    ax[1][0].set_ylim([0., 35.])
    ax[1][0].set_ylabel('inclination (deg)')
    ax[1][0].set_yticks(range(0, 40, 10))
    ax[1][0].grid(True, which='both', alpha=0.4)

    plot_fanciness.remove_border(ax[0][0])
    ax[0][0].tick_params(labelbottom='off')
    plot_fanciness.remove_border(ax[0][1])
    plot_fanciness.remove_border(ax[1][0])
    plot_fanciness.remove_border(ax[1][1], remove=['left', 'right', 'top', 'bottom'], keep=[])
    ax[1][1].tick_params(labelbottom='off', labelleft='off')

    plt.draw()
    outfile = 'OSSOS_v{}_classical_aei.pdf'.format(parameters.RELEASE_VERSION)
    plt.savefig(outfile, transparent=True, bbox_inches='tight')


def argperi_a(directory):
    # dir = '/Users/bannisterm/Dropbox/OSSOS/measure3/test_argperi_align/'
    # directory = '/Users/bannisterm/Dropbox/OSSOS/measure3/2014B-H/track/'
    tnos = parsers.ossos_discoveries(directory=directory, all_objects=True)

    fig, ax = plt.subplots(3, 1, sharex=True, figsize=(7, 8))  # a4 is 1 x sqrt(2), so use those proportions
    fig.subplots_adjust(hspace=0.25)

    for obj in tnos:
        if obj.orbit.arc_length < 60.*units.day:
            print 'Skipping', obj.name, obj.orbit.arc_length
            continue

        alpha = 0.4

        obj_r = obj.orbit.distance.value
        obj_dr = obj.orbit.distance_uncertainty.value
        obj_a = obj.orbit.a.value
        obj_da = obj.orbit.da.value
        obj_i = obj.orbit.inc.value
        obj_di = obj.orbit.dinc.value
        obj_e = obj.orbit.e.value
        obj_de = obj.orbit.de.value

        obj_peri = obj_a * (1. - obj_e)
        obj_dperi = (obj_da/obj_a) + (obj_de/obj_e)
        obj_argP = obj.orbit.om.value
        if obj_argP > 180.:
            obj_argP = obj_argP - 360.
        obj_dargP = obj.orbit.dom.value

        ax[0].errorbar(obj_r, obj_i,
                       xerr=obj_dr,
                       yerr=obj_di,
                       fmt='.', ms=10, color='b', alpha=alpha
                       )
        ax[1].errorbar(obj_a, obj_i,
                       xerr=obj_da,
                       yerr=obj_di,
                       fmt='.', ms=10, color='b', alpha=alpha
                       )
        ax[2].errorbar(obj_a, obj_e,
                       xerr=obj_da,
                       yerr=obj_de,
                       fmt='.', ms=10, color='b', alpha=alpha
                       )
        # ax[3].errorbar(obj_a, obj_argP,
        #                xerr=obj_dargP,
        #                yerr=obj_da,
        #                fmt='.', ms=10, color='b', alpha=alpha
        #                )

    ymin = -0.001
    imax = 50
    emax = .85
    xinner = 20
    xouter = 80
    xticker = 5
    grid_alpha = 0.2

    resonances(ax, imax, emax)

    ax[0].set_ylim([ymin, imax])
    ax[1].set_ylim([ymin, imax])
    ax[2].set_ylim([ymin, emax])
    # ax[3].set_ylim([-180, 180])
    plt.xlim([xinner, xouter])


    ax[0].set_xticks(range(xinner, xouter, xticker))
    ax[1].set_xticks(range(xinner, xouter, xticker))
    ax[2].set_xticks(range(xinner, xouter, xticker))

    plot_fanciness.remove_border(ax[0])
    plot_fanciness.remove_border(ax[1])
    plot_fanciness.remove_border(ax[2])
    # plot_fanciness.remove_border(ax[3])

    ax[0].set_ylabel('inclination (degrees)')
    ax[0].grid(True, alpha=grid_alpha)
    ax[0].set_xlabel('heliocentric distance (AU)')
    ax[1].set_ylabel('inclination (degrees)')
    ax[1].grid(True, alpha=grid_alpha)
    ax[2].set_ylabel('eccentricity')
    ax[2].grid(True, alpha=grid_alpha)
    # ax[2].set_xlabel('semimajor axis (AU)')
    plt.xlabel('semimajor axis (AU)')
    # ax[3].grid(True, alpha=grid_alpha)
    # ax[3].set_xlabel('perihelion (AU)')
    # ax[3].set_ylabel('arg. peri. (degrees)')

    plt.draw()
    # ofile = 'OSSOS+CFEPS+NGVS_aei_argperi.pdf'
    outfile = 'OSSOS_aei_{}.pdf'.format(parameters.RELEASE_VERSION)
    plt.savefig(outfile, transparent=True, bbox_inches='tight')


if __name__ == '__main__':
    tnos = parsers.ossos_release_parser(table=True)  # return as an astropy.Table.Table

    # argperi_a(parameters.REAL_KBO_AST_DIR)

    full_aei(tnos)
    # classicals_qi(tnos)

#     classicals = tnos[numpy.array([name.startswith('m') for name in tnos['p']])]
#     classicals_aei(classicals)