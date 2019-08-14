#include <bits/stdc++.h>

using namespace std;
//% Big Integer

//! BigInt
// Based on indy256: https://github.com/indy256/codelibrary/blob/master/cpp/numeric/bigint.cpp
struct BigInt {
	// Definitions:
	using u_t = uint32_t;
	using s_t = int32_t;
	using m_t = uint64_t;
	constexpr static int ndigits = 9;
	constexpr static u_t base = 1'000'000'000;
	using Vec = vector<u_t>;
	template <class T, class S>
	using SmallInt = enable_if_t<is_integral<T>::value and sizeof(T) <= sizeof(u_t), S>;
	// Member Variables:
	bool neg = false;
	Vec arr;
	// Constructors:
	BigInt() = default;
	BigInt(string const &s) { Parse(s.c_str(), s.size()); }
	BigInt(char const *s) { Parse(s, strlen(s)); }

	template <class T, class = enable_if_t<is_integral<T>::value>> BigInt(T x) : neg(x < 0) {
		if (neg) x = -x;
		while (x) {
			auto q = x / base;
			arr.push_back(x % base);
			x = q;
		}
	}

	void Clear() { neg = false, arr.clear(); }
	bool Zero() const { return arr.empty(); }
	size_t Size() const { return arr.size(); }
	size_t Capacity() const { return arr.capacity(); }

	BigInt operator-() const & { return arr.empty() ? BigInt{} : BigInt{!neg, arr}; }
	BigInt &&operator-() && {
		if (!arr.empty()) neg = !neg;
		return move(*this);
	}
	BigInt Abs() const & { return {false, arr}; }
	BigInt &&Abs() && { return neg = false, move(*this); }

	BigInt &operator+=(BigInt const &rhs) {
		if (neg == rhs.neg) return *this = UAdd(move(*this), rhs);
		return *this = USub(move(*this), rhs);
	}
	BigInt &operator-=(BigInt const &rhs) {
		if (neg == rhs.neg) return *this = USub(move(*this), rhs);
		return *this = UAdd(move(*this), rhs);
	}

	template <class T> SmallInt<T, BigInt &> operator*=(T v) {
		if (arr.empty() or v == 1) return *this;
		if (v == 0) return Clear(), *this;
		if (v < 0) neg = !neg, v = -v;
		if (v == 1) return *this;
		m_t carry = 0, c = v;
		for (u_t &a : arr) {
			carry += c * a;
			a = carry % base;
			carry /= base;
		}
		if (carry) {
			arr.push_back(carry % base);
			if (carry /= base) arr.push_back(carry);
		}
		return *this;
	}
	BigInt &operator*=(BigInt const &rhs) { return *this = Mult(*this, rhs); }

	template <class T> friend SmallInt<T, BigInt> operator*(BigInt lhs, T rhs) {
		return move(lhs *= rhs);
	}

	template <class T> friend SmallInt<T, BigInt> operator*(T lhs, BigInt rhs) {
		return move(rhs *= lhs);
	}

	friend BigInt operator*(BigInt const &lhs, BigInt const &rhs) { return Mult(lhs, rhs); }

	friend BigInt operator+(BigInt const &lhs, BigInt const &rhs) {
		if (lhs.neg == rhs.neg)
			return (lhs.Size() < rhs.Size()) ? UAdd(rhs, lhs) : UAdd(lhs, rhs);
		return USub(lhs, rhs);
	}
	friend BigInt operator+(BigInt &&lhs, BigInt &&rhs) {
		if (lhs.neg == rhs.neg)
			return (lhs.Capacity() < rhs.Capacity()) ? UAdd(move(rhs), lhs) : UAdd(move(lhs), rhs);
		return USub(move(lhs), move(rhs));
	}
	friend BigInt operator+(BigInt &&lhs, BigInt const &rhs) { return move(lhs += rhs); }
	friend BigInt operator+(BigInt const &lhs, BigInt &&rhs) { return move(rhs += lhs); }

	friend BigInt operator-(BigInt const &lhs, BigInt const &rhs) {
		if (lhs.neg == rhs.neg) return USub(lhs, rhs);
		return (lhs.Size() < rhs.Size()) ? UAdd(rhs, lhs) : UAdd(lhs, rhs);
	}
	friend BigInt operator-(BigInt &&lhs, BigInt &&rhs) { return move(lhs) + (-move(rhs)); }
	friend BigInt operator-(BigInt &&lhs, BigInt const &rhs) { return move(lhs -= rhs); }
	friend BigInt operator-(BigInt const &lhs, BigInt &&rhs) { return -move(rhs -= lhs); }

	friend bool operator==(BigInt const &lhs, BigInt const &rhs) {
		return lhs.neg == rhs.neg and lhs.arr == rhs.arr;
	}
	friend bool operator!=(BigInt const &lhs, BigInt const &rhs) {
		return lhs.neg != rhs.neg or lhs.arr != rhs.arr;
	}
	friend bool operator<(BigInt const &lhs, BigInt const &rhs) {
		if (lhs.neg) {
			if (!rhs.neg) return true;
			return UComp(rhs, lhs) < 0;
		} else {
			if (rhs.neg) return false;
			return UComp(lhs, rhs) < 0;
		}
	}
	friend bool operator>(BigInt const &lhs, BigInt const &rhs) { return rhs < lhs; }
	friend bool operator<=(BigInt const &lhs, BigInt const &rhs) { return !(rhs < lhs); }
	friend bool operator>=(BigInt const &lhs, BigInt const &rhs) { return !(lhs < rhs); }

	template <class T> friend SmallInt<T, pair<BigInt, T>> Div(BigInt A, T b) {
		if (b < 0) A.neg = !A.neg, b = -b;
		m_t carry = 0;
		for (size_t i = A.Size(); i--;) {
			carry = carry * base + A[i];
			A[i] = carry / b;
			carry %= b;
		}
		A.pop0s();
		return {move(A), carry};
	}

	template <class T> SmallInt<T, T> operator%(T b) const {
		if (b < 0) b = -b;
		m_t carry = 0;
		for (size_t i = arr.size(); i--;) carry = (carry * base + arr[i]) % b;
		return neg ? -carry : carry;
	}

	template <class T> SmallInt<T, BigInt &> operator/=(T rhs) {
		return *this = Div(move(*this), rhs).first;
	}

	template <class T> friend SmallInt<T, BigInt> operator/(BigInt lhs, T rhs) {
		return Div(move(lhs), rhs).first;
	}

	template <class T> SmallInt<T, BigInt &> operator%=(T rhs) { return *this = (*this % rhs); }

	BigInt &operator/=(BigInt const &rhs) { return *this = Div(*this, rhs).first; }
	BigInt &operator%=(BigInt const &rhs) { return *this = Div(*this, rhs).second; }

	friend BigInt operator/(BigInt const &lhs, BigInt const &rhs) { return Div(lhs, rhs).first; }
	friend BigInt operator%(BigInt const &lhs, BigInt const &rhs) { return Div(lhs, rhs).second; }

	// B non zero
	friend pair<BigInt, BigInt> Div(BigInt const &A, BigInt const &B) {
		if (B.Zero()) throw runtime_error("Division by zero");
		u_t norm = base / (B.arr.back() + 1);
		const BigInt a = A.Abs() * norm;
		const BigInt b = B.Abs() * norm;
		BigInt q, r;
		q.arr.resize(a.Size());
		for (size_t i = a.Size(); i--;) {
			r *= base;
			r += a[i];
			u_t s1 = b.Size() < r.Size() ? r[b.Size()] : 0;
			u_t s2 = b.Size() < r.Size() + 1 ? r[b.Size() - 1] : 0;
			u_t d = (static_cast<m_t>(base) * s1 + s2) / b.arr.back();
			r -= b * d;
			while (r.neg) r += b, d--;
			q[i] = d;
		}
		q.neg = (A.neg != B.neg);
		r.neg = A.neg;
		q.pop0s();
		return {move(q), move(r) / norm};
	}

	// Input Output:
	friend ostream &operator<<(ostream &os, BigInt const &x) {
		if (x.arr.empty()) return os << 0;
		if (x.neg) os << '-';
		os << x.arr.back();
		for (size_t i = x.arr.size() - 1; i--;) {
			os << setw(ndigits) << setfill('0') << x.arr[i];
		}
		return os;
	}

	friend istream &operator>>(istream &is, BigInt &x) {
		string s;
		is >> s;
		x.Clear();
		x.Parse(s.c_str(), s.size());
		return is;
	}

	long long ToLongLong() const {
		long long res = 0;
		for (size_t i = arr.size(); i--;) res = res * base + arr[i];
		return neg ? -res : res;
	}

	friend BigInt Gcd(BigInt a, BigInt b) {
		while (!b.Zero()) {
			auto tmp = a % b;
			a = move(b);
			b = move(tmp);
		}
		return a;
	}

  private:
	u_t const &operator[](size_t i) const { return arr[i]; }
	u_t &operator[](size_t i) { return arr[i]; }

	BigInt(bool neg, Vec const &v) : neg(neg), arr(v) {}
	BigInt(bool neg, Vec &&v) : neg(neg), arr(move(v)) {}

	// precondition: needs to be cleared
	void Parse(char const *s, size_t n) {
		size_t pos = 0;
		if (s[0] == '-') neg = true, pos = 1;
		if (s[0] == '+') pos = 1;
		arr.resize((n - pos + ndigits - 1) / ndigits);
		for (size_t i = n, c = 0; i > pos; c++) {
			const size_t k = (i > pos + ndigits) ? i - ndigits : pos;
			u_t x = 0;
			for (size_t j = k; j < i; j++) {
				x = x * 10 + s[j] - '0';
			}
			arr[c] = x;
			i = k;
		}
		pop0s();
	}

	void pop0s() {
		size_t n = arr.size();
		while (n and arr[n - 1] == 0) n--;
		arr.resize(n);
		if (arr.empty()) neg = false;
	}

	// unsigned add:
	static BigInt UAdd(BigInt A, BigInt const &B) {
		const size_t bs = B.arr.size();
		if (A.Size() < bs) A.arr.resize(bs);
		u_t carry = 0;
		for (size_t i = 0; i < bs; i++) {
			A[i] += B[i] + carry;
			carry = (A[i] >= base);
			if (carry) A[i] -= base;
		}
		for (size_t i = bs; carry and i < A.Size(); i++) {
			if (++A[i] < base)
				carry = 0;
			else
				A[i] -= base;
		}
		if (carry) A.arr.push_back(1);
		return A;
	}

	// keeps sign of the greater BigInt
	template <class B1, class B2> static BigInt USub(B1 &&A, B2 &&B) {
		int c = UComp(A, B);
		if (c == 0) return {};
		if (c < 0) return {!A.neg, USubImpl(forward<B2>(B), A).arr};
		return USubImpl(forward<B1>(A), B);
	}

	// precondition A > B
	static BigInt USubImpl(BigInt A, BigInt const &B) {
		u_t carry = 0;
		for (size_t i = 0; i < B.Size(); i++) {
			A[i] -= B[i] + carry;
			if (static_cast<s_t>(A[i]) < 0)
				A[i] += base, carry = 1;
			else
				carry = 0;
		}
		for (size_t i = B.Size(); carry; i++) {
			if (static_cast<s_t>(--A[i]) >= 0)
				carry = 0;
			else
				A[i] += base;
		}
		A.pop0s();
		return A;
	}

	// 0 if A==B, -1 if A < B, 1 if A > B
	static int UComp(BigInt const &A, BigInt const &B) {
		if (A.Size() < B.Size()) return -1;
		if (B.Size() < A.Size()) return 1;
		for (size_t i = A.Size(); i--;) {
			if (A[i] < B[i]) return -1;
			if (B[i] < A[i]) return 1;
		}
		return 0;
	}

	static BigInt Mult(BigInt const &A, BigInt const &B) {
		if (A.Zero() or B.Zero()) return {};
		Vec res(A.Size() + B.Size());
		for (size_t i = 0; i < A.Size(); i++) {
			m_t carry = 0;
			const m_t a = A[i];
			size_t k = i;
			for (size_t j = 0; j < B.Size(); j++, k++) {
				carry += res[k] + a * B[j];
				res[k] = carry % base;
				carry /= base;
			}
			res[k] = carry;
		}
		BigInt C(A.neg != B.neg, res);
		return C.pop0s(), C;
	}
};
