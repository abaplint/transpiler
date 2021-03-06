export function getDateFromNumber(value: number): string {
  const msInOneDay = 24 * 60 * 60 * 1000;
  const date = new Date(-62135596800000 + value * msInOneDay);
  let removeJulianLeaps = 2;
  if (value <= 577736) {
    let beforeGregorian = date.getFullYear() <= 1582 ? date.getFullYear() : 1582;
    if (date.getMonth() < 1 || ( date.getMonth() === 1 && date.getDay() < 29)) {
      beforeGregorian -= 1;
    }
    removeJulianLeaps = Math.floor( beforeGregorian / 100 ) - Math.floor( beforeGregorian / 400 );
  }

  date.setTime(date.getTime() - removeJulianLeaps * msInOneDay);

  let ret = date.getFullYear().toString().padStart(4,"0");
  ret += (date.getMonth() + 1).toString().padStart(2,"0");
  ret += date.getDate().toString().padStart(2,"0");

  return ret;
}

export function getNumberFromDate(value: string): number {
  const msInOneDay = 24 * 60 * 60 * 1000;
  const date = new Date(-62135596800000);

  date.setUTCFullYear(parseInt(value.substr(0,4),10));
  date.setUTCMonth(parseInt(value.substr(4,2),10) - 1);
  date.setUTCDate(parseInt(value.substr(6,2),10));

  let days = Math.floor((date.getTime() + 62135596800000 ) / msInOneDay);
  let addJulianLeaps = 2;
  if (days <= 577736) {
    let beforeGregorian = date.getFullYear() <= 1582 ? date.getFullYear() : 1582;
    if (date.getMonth() < 1 || ( date.getMonth() === 1 && date.getDay() < 29)) {
      beforeGregorian -= 1;
    }
    addJulianLeaps = Math.floor( beforeGregorian / 100 ) - Math.floor( beforeGregorian / 400 );
  }
  days = days + addJulianLeaps;
  return days;
}