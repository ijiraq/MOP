__author__ = "David Rusk <drusk@uvic.ca>"

import math

PI180 = 57.2957795130823208767981548141052


def get_order(pv):
    """
    Determine the order of a PV matrix.

    Args:
      pv: 2d array
        PV coefficients.

    Returns:
      order: int
        1 for linear, 2
    """
    if len(pv) != 2:
        raise ValueError("PV matrix must have 2 rows, but had: %d" % len(pv))

    # TODO should check all rows same length and throw exception if not
    row_len = len(pv[0])

    if row_len == 1:
        return 0
    elif row_len == 4:
        return 1
    elif row_len == 7:
        return 2
    elif row_len == 11:
        return 3
    else:
        raise ValueError("PV matrix has unknown order: "
                         "%d coefficients per row" % row_len)


def xy2sky(x, y, crpix1, crpix2, crval1, crval2, cd, pv, nord):
    """
    Transforms from pixel coordinates to celestial coordinates taking
    non-linear distortion into account with the World Coordinate System
    FITS keywords as used in MegaPipe.

    See: http://www.cadc-ccda.hia-iha.nrc-cnrc.gc.ca/megapipe/docs/CD_PV_keywords.pdf

    Args:
      x, y: int
        Input pixel coordinate
      crpix1: float
        Tangent point x, pixels
      crpix2: float
        Tangent point y, pixels
      crval1: float
        Tangent point RA, degrees
      crval2:
        Tangent point Dec, degrees
      cd: 2d array
        Expresses the scale, the rotation and any possible skew of the image
        with respect to the sky.
      pv: 2d array
      nord: int
        order of the fit

    Returns:
      ra: float
        Right ascension
      dec: float
        Declination
    """
    xp = x - crpix1
    yp = y - crpix2

    print "xp=%.16e, yp=%.16e" % (xp, yp)

    # IMPORTANT NOTE: 0-based indexing in Python means indexing for values
    # in cd and pv will be shifted from in the paper.
    x_deg = cd[0][0] * xp + cd[0][1] * yp
    y_deg = cd[1][0] * xp + cd[1][1] * yp

    if nord < 0:
        xi = x
        eta = y

    if nord >= 0:
        xi = pv[0][0]
        eta = pv[1][0]

    print ">=0: xi=%.16e, eta=%.16e" % (xi, eta)

    if nord >= 1:
        r = math.sqrt(x_deg ** 2 + y_deg ** 2)
        xi += pv[0][1] * x_deg + pv[0][2] * y_deg + pv[0][3] * r
        eta += pv[1][1] * y_deg + pv[1][2] * x_deg + pv[1][3] * r

    if nord >= 2:
        x2 = x_deg ** 2
        xy = x_deg * y_deg
        y2 = y_deg ** 2

        xi += pv[0][4] * x2 + pv[0][5] * xy + pv[0][6] * y2
        eta += pv[1][4] * y2 + pv[1][5] * xy + pv[1][6] * x2

    if nord >= 3:
        x3 = x_deg ** 3
        x2y = x2 * y_deg
        xy2 = x_deg * y2
        y3 = y_deg ** 3

        xi += pv[0][7] * x3 + pv[0][8] * x2y + pv[0][9] * xy2 + pv[0][10] * y3
        eta += pv[1][7] * y3 + pv[1][8] * xy2 + pv[1][9] * x2y + pv[1][10] * x3

    xir = float(xi) / PI180
    etar = float(eta) / PI180

    ra0 = crval1 / PI180
    dec0 = crval2 / PI180

    ctan = math.tan(dec0)
    ccos = math.cos(dec0)
    raoff = math.atan2(xir / ccos, 1 - etar * ctan)
    ra = raoff + ra0
    dec = math.atan(math.cos(raoff) / ((1 - (etar * ctan)) / (etar + ctan)))

    ra *= PI180
    if ra < 0:
        ra += 360
    if ra >= 360:
        ra -= 360

    dec *= PI180

    return ra, dec
