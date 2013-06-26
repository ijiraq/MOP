__author__ = "David Rusk <drusk@uvic.ca>"

import os

from pymop.io.persistence import FileLockedException
from pymop.io.astrom import Source


class NoAvailableWorkException(Exception):
    """"No more work is available."""


class WorkCompleteException(Exception):
    """Everything in the WorkUnit has been processed."""


class VettableItem(object):
    ACCEPTED = "accepted"
    REJECTED = "rejected"
    UNPROCESSED = "unprocessed"

    def __init__(self, item):
        self._status = VettableItem.UNPROCESSED
        self.item = item

    def __getattr__(self, attr):
        return getattr(self.item, attr)

    def is_processed(self):
        return self._status != VettableItem.UNPROCESSED

    def is_accepted(self):
        return self._status == VettableItem.ACCEPTED

    def is_rejected(self):
        return self._status == VettableItem.REJECTED

    def accept(self):
        self._status = VettableItem.ACCEPTED

    def reject(self):
        self._status = VettableItem.REJECTED

    def get_status(self):
        return self._status


class StatefulCollection(object):
    def __init__(self, items):
        self.items = items
        self.index = 0

    def __len__(self):
        return len(self.items)

    def __iter__(self):
        return iter(self.items)

    def __getattr__(self, attr):
        return getattr(self.items, attr)

    def get_index(self):
        return self.index

    def get_current_item(self):
        return self.items[self.index]

    def next(self):
        self._move(1)

    def previous(self):
        self._move(-1)

    def reset(self):
        self.index = 0

    def _move(self, delta):
        self.index = (self.index + delta) % len(self)

    def get_items(self):
        return self.items


class WorkUnit(object):
    def __init__(self, filename, data_collection):
        self.filename = filename
        self.data_collection = data_collection

    def get_filename(self):
        return self.filename

    def get_sources(self):
        return self.data_collection.get_sources()

    def get_source_count(self):
        return len(self.get_sources())

    def get_current_source(self):
        return self.get_sources().get_current_item()

    def get_current_source_number(self):
        return self.get_sources().get_index()

    def get_current_source_readings(self):
        return self.get_current_source().get_readings()

    def get_obs_count(self):
        return len(self.get_current_source_readings())

    def get_current_reading(self):
        return self.get_current_source_readings().get_current_item()

    def get_current_obs_number(self):
        return self.get_current_source_readings().get_index()

    def next_source(self):
        self.get_sources().next()

    def previous_source(self):
        self.get_sources().previous()

    def next_obs(self):
        self.get_current_source_readings().next()

    def previous_obs(self):
        self.get_current_source_readings().previous()

    def accept_current_item(self):
        raise NotImplementedError()

    def reject_current_item(self):
        raise NotImplementedError()

    def next_vettable_item(self):
        raise NotImplementedError()


class RealsWorkUnit(WorkUnit):
    def __init__(self, filename, data_collection):
        super(RealsWorkUnit, self).__init__(filename, data_collection)

    def accept_current_item(self):
        self.get_current_reading().accept()

    def reject_current_item(self):
        self.get_current_reading().accpet()

    def next_vettable_item(self):
        num_obs_to_check = len(self.get_current_source_readings())
        while num_obs_to_check > 0:
            self.next_obs()
            if not self.get_current_reading().is_processed():
                # This observation needs to be vetted, stop here
                return

        # All observations of this source have been processed
        self.next_source()


class CandidatesWorkUnit(WorkUnit):
    def __init__(self, filename, data_collection):
        super(CandidatesWorkUnit, self).__init__(filename, data_collection)

    def accept_current_item(self):
        self.get_current_source().accept()

    def reject_current_item(self):
        self.get_current_source().reject()

    def next_vettable_item(self):
        self.next_source()


class WorkUnitProvider(object):
    def __init__(self,
                 taskid,
                 directory_manager,
                 progress_manager,
                 builder):
        self.taskid = taskid
        self.directory_manager = directory_manager
        self.progress_manager = progress_manager
        self.builder = builder

    def get_workunit(self):
        potential_files = self.directory_manager.get_listing(self.taskid)

        while len(potential_files) > 0:
            potential_file = potential_files.pop()

            if not self.progress_manager.is_done(potential_file):
                try:
                    self.progress_manager.lock(potential_file)
                except FileLockedException:
                    continue

                return self.builder.build_workunit(
                    self.directory_manager.get_full_path(potential_file))

        raise NoAvailableWorkException()


class DataCollection(object):
    def __init__(self, parsed_data):
        self.observations = parsed_data.observations
        self.sys_header = parsed_data.sys_header

        sources = []
        for source in parsed_data.get_sources():
            reading_collection = StatefulCollection(
                map(VettableItem, source.get_readings()))
            sources.append(VettableItem(Source(reading_collection)))

        self.source_collection = StatefulCollection(sources)

    def get_sources(self):
        return self.source_collection


class WorkUnitBuilder(object):
    def __init__(self, parser):
        self.parser = parser

    def build_workunit(self, full_path):
        work_items = self._create_work_items(self.parser.parse(full_path))
        _, filename = os.path.split(full_path)
        return WorkUnit(filename, work_items)

    def _create_work_items(self, parsed_data):
        raise NotImplementedError()


class CandidatesWorkUnitBuilder(WorkUnitBuilder):
    def __init__(self, parser):
        super(CandidatesWorkUnitBuilder, self).__init__(parser)

    def _create_work_items(self, parsed_data):
        return [VettableItem(source) for source in parsed_data.get_sources()]


class RealsWorkUnitBuilder(WorkUnitBuilder):
    def __init__(self, parser):
        super(RealsWorkUnitBuilder, self).__init__(parser)

    def _create_work_items(self, parsed_data):
        return [VettableItem(reading) for source in parsed_data.get_sources()
                for reading in source]


class WorkloadManager(object):
    """
    Manages the workload's state.
    """

    def __init__(self, workunit_provider):
        self.workunit_provider = workunit_provider
        self.current_workunit = None
        self.workunit_number = 0

    def next_workunit(self):
        pass

    def previous_workunit(self):
        pass

    def get_current_filename(self):
        pass

    def get_current_data(self):
        pass


class DirectoryManager(object):
    def __init__(self, directory):
        self.directory = directory

    def get_listing(self, suffix):
        return listdir_for_suffix(self.directory, suffix)

    def get_full_path(self, filename):
        return os.path.join(self.directory, filename)


def listdir_for_suffix(directory, suffix):
    """Note this returns file names, not full paths."""
    return filter(lambda name: name.endswith(suffix), os.listdir(directory))
