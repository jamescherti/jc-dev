##!/usr/bin/env python
#
# Author: James Cherti
# URL: https://github.com/jamescherti/jc-dev
#
# Copyright (C) 2004-2026 James Cherti
#
# This program is free software: you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation, either version 3 of the License, or (at your option) any later
# version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
# details.
#
# You should have received a copy of the GNU General Public License along with
# this program. If not, see <https://www.gnu.org/licenses/>.
#
"""Manage financial operations."""

import re
from collections import defaultdict
from datetime import date as Date

import yaml
from dateutil.parser import parse as parse_date
from prettytable import PrettyTable

from accounts import Accounts, AccountUsed
from config import DEFAULT_ACCOUNT, DEFAULT_CURRENCY

# The index of the operations in Operations.oplist
I_DATE = 0
I_AMOUNT = 1
I_DESC = 2
I_ACCOUNT = 3


class Operations:
    """List of financial operations."""

    def __init__(self, default_account=DEFAULT_ACCOUNT,
                 default_currency=DEFAULT_CURRENCY):
        """Initialize the Operations.

        if default_account is None, then the default account
        is not set
        """
        self.accounts = Accounts()
        self.oplist = []    # self.clear()
        if default_account:
            self.accounts.add(default_account, default_currency)

    def clear(self):
        """Remove all operation."""
        self.oplist = []
        self.accounts.clear()

    def id_exists(self, opid):
        """Check if the operation id exists."""
        length = len(self.oplist)
        if opid >= length or opid < 0:
            return False
        else:
            return True

    def rename_account(self, account, new_account):
        """Rename the account name from the old name (acount) to new_account.

        KeyError could be raised if the account doesn't exist
        or new_account exists.
        """
        if not self.accounts.exists(account):
            raise KeyError("The account '%s' doesn't exist" % account)

        # cound raise an exception (KeyError)
        self.accounts.rename(account, new_account)

        account = self.accounts.norm_account_name(account)
        for index in range(len(self.oplist)):
            if self.oplist[index][I_ACCOUNT] == account:
                self.oplist[index][I_ACCOUNT] = new_account

    def remove_account(self, account, force=False):
        """Remove an account + all operations related to this account.

        Arguments:
            account: if not None, remove all operation the account
                     account (+ the account)
            force: if False, the account will not be removed except
                   if no operation is assigned to this account. If
                   there is operations inside the account,
                   the exception AccountUsed will be raised.

        Exceptions:
            KeyError if the account doesn't exist
        """
        if not self.accounts.exists(account):
            raise KeyError("The account '%s' doesn't exist" % account)

        account = self.accounts.norm_account_name(account)

        if not force:
            for oper in self.oplist:
                if oper[I_ACCOUNT] == account:
                    raise AccountUsed

        self.accounts.remove(account)

        for index in range(len(self.oplist)):
            if self.oplist[index][I_ACCOUNT] == account:
                del self.oplist[index][I_ACCOUNT]

    def remove(self, opid=None):
        """Remove an operation.

        Arguments:
            opid: the id if the account

        Exceptions:
            KeyError if the opid doesn't exist
        """
        del self.oplist[opid]

    def __find_insindex(self, date, amount):
        """Get the index were we can insert a date (YYYY-MM-DD format).

        IMPORTANT: do a normalize_date on date before calling this function.
        It returns the index where we can insert.

        Arguments:
            date = YYYY-MM-DD
            amount = float / int
        """
        date_index = len(self.oplist)
        date = parse_date(date)
        for index in range(len(self.oplist)):
            oplist_date = parse_date(self.oplist[index][I_DATE])
            if oplist_date < date:
                date_index = index + 1
                # organize (positives in the beginning and
                # negative after them)
            elif oplist_date == date:
                date_index = index + 1
                if amount >= 0 and self.oplist[index][I_AMOUNT] < 0:
                    date_index = index    # previous one
                    break
            else:
                date_index = index
                break

        return date_index

    def modify(self, opid, date=None, amount=None, desc=None,
               account=None):
        """Modify an entry.

        Arguments:
            opid is the operation id (index self.oplist)
            desc=None if none, the desc will not be modified
            The other arguments are the same as self.add()

        Exceptions:
            IndexError    if "opid" doesn't exit

        """
        return self.add(date, amount, desc, account, modify_id=opid)

    def add(self, date, amount, desc='', account=None, organize=True,
            modify_id=None):
        """Add an operation.

        Arguments:
            date = 2015-01-01
            amount = 12
            desc = description of the operation
            account = main (if None, the default account is selected)
            organize = True / False (sort the operation by date)
                       When organize=True, the operations will be organized
                       automatically (sorted by date and per positive /
                       negative numbers)
            modify_id = if this id is set, the function will modify, instead
                        of add (organize will be ignored)

        Possible exceptions:
            ValueError    (if date is not a good date or the account doesn't
                           exist)

        It returns the index of the operation
        """
        if date is not None:
            assert isinstance(date, str)
            try:
                date = self.normalize_date(date)
            except UnicodeDecodeError:
                raise ValueError("The date is incorrect")

        if isinstance(amount, str):
            amount = float(amount)
            if amount == int(amount):
                amount = int(amount)
        elif amount is not None and isinstance(amount, float) \
                and isinstance(amount, int):
            raise ValueError("The amount needs to be float or an int")

        if desc is not None:
            assert isinstance(desc, str)

        if account is not None:
            assert isinstance(account, str)

        assert isinstance(organize, bool)

        if account and not self.accounts.exists(account):
            raise ValueError("The account '%s' doesn't exist" % account)

        if modify_id is None:
            # add
            if date is None:
                date = self.oplist[modify_id][I_DATE]
            if amount is None:
                amount = 0
            if desc is None:
                desc = ""
            if account is None:
                account = self.accounts.default
                if account is None:
                    raise ValueError("You need to specify the account "
                                     "(or a default account)")
        else:
            # modify
            if date is None:
                date = self.oplist[modify_id][I_DATE]
            if amount is None:
                amount = self.oplist[modify_id][I_AMOUNT]
            if desc is None:
                desc = self.oplist[modify_id][I_DESC]
            if account is None:
                account = self.oplist[modify_id][I_ACCOUNT]

            self.remove(modify_id)

        index = self.__find_insindex(date, amount) \
            if organize else len(self.oplist)
        entry = [date, amount, desc,
                 self.accounts.norm_account_name(account)]

        self.oplist.insert(index, entry)

        return index

    def filter(self, year=None, month=None, day=None):
        """Find an operation by date.

        Arguments: year / month / day are integers
        If year one of them is None, then it will be
        ignored in the filter.
        """
        year = int(year) if isinstance(year, str) else year
        month = int(month) if isinstance(month, str) else month
        day = int(day) if isinstance(day, str) else day

        result = []
        for oper in self.oplist:
            date = parse_date(oper[I_DATE])
            if year and date.year != year:
                continue

            if month and date.month != month:
                continue

            if day and date.day != day:
                continue

            result.append(oper)

        return result

    @staticmethod
    def is_date(date):
        """It returns true if date is a date in this format: YYYY-MM-DD."""
        return True if re.match(r'[0-9]{4}-[0-9]{2}-[0-9]{2}', date) else False

    @staticmethod
    def normalize_case(string):
        """It will read the date from the string, parse it and format it.

        Return a string: YYYY-MM-DD (the value returned is a string)

        Possible exceptions:
            ValueError    (if the date is incorrect)

        date could be a date (datetime) or a string with a date
        """
        string = string.lower().strip()
        if len(string) > 0:
            string = "%s%s" % (str(string[0]).upper(), string[1:])

        return string

    @staticmethod
    def normalize_date(date):
        """It will read the date from the string, parse it and format it.

        Return a string YYYY-MM-DD (the value returned is a string)

        Possible exceptions:
            ValueError    (if the date is incorrect)

        date could be a date (datetime) or a string with a date
        """
        if isinstance(date, str):
            date = parse_date(date)

        if date:
            return date.strftime('%Y-%m-%d')
        else:
            raise ValueError

    def load(self, filename, clear=True):
        """Load the operations.

        Possible exceptions:
            IOError
            yaml.scanner.ScannerError    (error in the file format)
        """
        fhandler = open(filename, 'r')
        oplist = yaml.load(fhandler, Loader=yaml.FullLoader)
        if oplist is None:
            # Nothing to do
            fhandler.close()
            return

        try:
            if clear:
                self.clear()

            if len(oplist) != 2 or oplist[0] != 'PF-OP-FORMAT':
                raise IndexError

            # add them with our function (the strings will be lower case)
            for account in list(oplist[1]['accounts'].keys()):
                self.accounts.accounts[account] = \
                    oplist[1]['accounts'][account]
            self.accounts.default = oplist[1]['accounts_default']

            self.oplist = self.oplist + oplist[1]['operations']
        except IndexError:
            raise yaml.scanner.ScannerError  # file format error
        finally:
            fhandler.close()

    def save(self, filename):
        """Save all operations.

        Possible exceptions:
            IOError

        """
        acc = self.accounts
        content = ['PF-OP-FORMAT', {'accounts': acc.accounts,
                                    'accounts_default': acc.default,
                                    'operations': self.oplist}]
        content = yaml.dump(content)

        fhandler = open(filename, 'w')
        fhandler.write(content)
        fhandler.close()

    def monthly_results(self):
        """Get the monthly incomes, expenses and totals.

        It returns:
        {'<YEAR>': {'<MONTH>': {'CURRENCY': [income, expense, total,
                                             percent_earned]}}

        """
        result = {}

        for oper in self.oplist:
            date = parse_date(oper[I_DATE])
            year = date.year
            month = date.month

            # Append account
            cur = self.accounts[oper[I_ACCOUNT]]
            if cur not in result:
                result[cur] = {}

            if year not in result[cur]:
                result[cur][year] = {}

            if month not in result[cur][year]:
                # income, expense, total, percent_kept
                result[cur][year][month] = [0, 0, 0, 0]

            result[cur][year][month][2] += oper[I_AMOUNT]
            total = result[cur][year][month][2]
            if oper[I_AMOUNT] > 0:
                result[cur][year][month][0] += oper[I_AMOUNT]
            else:
                result[cur][year][month][1] += oper[I_AMOUNT]

            income = result[cur][year][month][0]
            percent_kept = 0
            try:
                percent_kept = round(total * 100 / income, 2)
            except ZeroDivisionError:
                percent_kept = 0
            finally:
                result[cur][year][month][3] = percent_kept

        return result

    def show_monthly_results(self):
        """Show monthly results, per currency (incomes, expenses and total)."""
        empty_row = ['', '', '', '', '', '']
        head_row = ['Date', 'Income', 'Expense', 'Monthly total',
                    '% kept', 'Total']

        result = self.monthly_results()
        for cur in sorted(result.keys()):
            big_total = 0
            print("Currency: %s" % cur)
            print()
            pre = PrettyTable(head_row)

            for year in result[cur]:
                for month in result[cur][year]:
                    date = "%s-%s" % (str(year), str(month))
                    income = round(result[cur][year][month][0], 2)
                    expense = round(result[cur][year][month][1], 2)
                    total = round(result[cur][year][month][2], 2)
                    percent_kept = "%i%%" % int(result[cur][year][month][3])

                    big_total += total
                    pre.add_row([date, income, expense, total, percent_kept,
                                 round(big_total, 2)])

            pre.add_row(empty_row)
            pre.add_row(head_row)

            print(pre)
            print()

    def yearly_results(self):
        """Get the yearly incomes, expenses and totals.

        It returns a defaultdict:
        {'<YEAR>': {'CURRENCY': [income, expense, total, percent_earned]}}

        """
        monthly = self.monthly_results()
        result = defaultdict(dict)

        for currency in monthly:
            for year in monthly[currency]:
                months = monthly[currency][year]
                year_result = [0, 0, 0, 0]

                # year_result = sum of all months
                for index in range(len(year_result)):
                    year_result[index] = sum([months[month][index]
                                              for month in months])

                # average
                percent_kept = 0
                income = year_result[0]
                total = year_result[2]
                try:
                    percent_kept = round(total * 100 / income, 2)
                except ZeroDivisionError:
                    percent_kept = 0
                finally:
                    year_result[3] = percent_kept

                # put the result!
                result[currency][year] = year_result

        return dict(result)

    def show_yearly_results(self):
        """Show yearly results, per currency (incomes, expenses and total)."""
        empty_row = ['', '', '', '', '', '']
        head_row = ['Year', 'Income', 'Expense', 'Yearly total',
                    '% kept', 'Total']

        result = self.yearly_results()
        for cur in sorted(result.keys()):
            big_total = 0
            print("Currency: %s" % cur)
            print()
            pre = PrettyTable(head_row)

            for year in result[cur]:
                date = str(year)
                income = round(result[cur][year][0], 2)
                expense = round(result[cur][year][1], 2)
                total = round(result[cur][year][2], 2)
                percent_kept = "%i%%" % int(result[cur][year][3])

                big_total += total
                pre.add_row([date, income, expense, total, percent_kept,
                             round(big_total, 2)])

            pre.add_row(empty_row)
            pre.add_row(head_row)

            print(pre)
            print()

    def show(self, year=None, month=None, day=None,
             begin=None, end=None, show_total=True):
        """Show the operations in a prettytable.

        Arguments:
        ----------
        year/month/day
            Could be None / String or Integer
            If year, month or day is specified, it will show the
            operations of the day / month / year. None means all
            of them (year=None=all years).

        begin: The first operation id we will show

        end: The last operation id we will show

        """
        year = int(year) if isinstance(year, str) else year
        month = int(month) if isinstance(month, str) else month
        day = int(day) if isinstance(day, str) else day

        # set the date
        today = Date.today()
        if month and not year:
            year = today.year    # current year, if month is set

        if day and not month:
            month = today.month   # current month, if day is set
            year = today.year     # current year, if month is set

        # tables were we will put each column data
        total = {'all': {}, 'income': {}, 'expense': {}}
        currency_income = {}    # {'cad': 1120}
        currency_expense = {}    # {'cad': 1120}

        # The table's header
        row_title = ["Date"]
        for account in self.accounts.get():
            row_title.append(account)
        row_title.append('Desc')
        row_title.append('ID')

        # empty row
        empty_row = []
        for index in range(len(row_title)):
            empty_row.append('')

        pre = PrettyTable(row_title)

        # read all operations
        begin = 0 if not begin or begin <= 0 else begin
        end = len(self.oplist) - 1 if not end or end > len(self.oplist) \
            else end

        for index in range(begin, end + 1):
            oper = self.oplist[index]

            cur_date = parse_date(oper[I_DATE])
            if year and cur_date.year != year:
                continue

            if month and cur_date.month != month:
                continue

            if day and cur_date.day != day:
                continue

            # Append account
            row = [oper[I_DATE]]
            for account in self.accounts.get():
                if account not in total['all']:
                    total['all'][account] = 0
                if account not in total['income']:
                    total['income'][account] = 0
                if account not in total['expense']:
                    total['expense'][account] = 0

                currency = self.accounts[account]
                if currency not in currency_income:
                    currency_income[currency] = 0
                if currency not in currency_expense:
                    currency_expense[currency] = 0

                if account == oper[I_ACCOUNT]:
                    row.append(oper[I_AMOUNT])
                    total['all'][account] += oper[I_AMOUNT]
                    if oper[I_AMOUNT] > 0:
                        total['income'][account] += oper[I_AMOUNT]
                        currency_income[currency] += oper[I_AMOUNT]
                    else:
                        total['expense'][account] += oper[I_AMOUNT]
                        currency_expense[currency] += oper[I_AMOUNT]
                else:
                    row.append('')

            row.append(oper[I_DESC])
            row.append(index)
            pre.add_row(row)

        if show_total:
            # empty
            pre.add_row(empty_row)

            # incomes
            row = ['Incomes']
            for account in self.accounts.get():
                if account in total['income']:
                    row.append(round(total['income'][account], 2))
                else:
                    row.append(0)
            row.append(' ')
            row.append(' ')
            pre.add_row(row)

            # expenses
            row = ['Expenses']
            for account in self.accounts.get():
                if account in total['income']:
                    row.append(round(total['expense'][account], 2))
                else:
                    row.append(0)
            row.append(' ')
            row.append(' ')
            pre.add_row(row)

            # total
            row = ['Total']
            for account in self.accounts.get():
                if account in total['income']:
                    row.append(round(total['all'][account], 2))
                else:
                    row.append(0)
            row.append(' ')
            row.append(' ')
            pre.add_row(row)

            # total for currency
            for currency in list(currency_income.keys()):
                pre.add_row(empty_row)
                for optype in ['Incomes', 'Expenses']:
                    row = ['%s %s' % (currency, optype)]

                    if optype == 'Incomes':
                        row.append(round(currency_income[currency], 2))
                    else:
                        row.append(round(currency_expense[currency], 2))

                    len_accounts = len(list(self.accounts.accounts.keys()))
                    if len_accounts > 0:
                        len_accounts -= 1
                        for account in range(len_accounts):
                            row.append('')

                    row.append(' ')
                    row.append(' ')
                    pre.add_row(row)

                row = ['%s Total' % currency]
                row.append(round(currency_expense[currency] +
                                 currency_income[currency], 2))
                len_accounts = len(list(self.accounts.accounts.keys()))
                if len_accounts > 0:
                    len_accounts -= 1
                    for account in range(len_accounts):
                        row.append('')
                row.append(' ')
                row.append(' ')
                pre.add_row(row)

        print(pre)

# vim:ai:et:sw=4:ts=4:sts=4:tw=78:fenc=utf-8
